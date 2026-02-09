{-# LANGUAGE FlexibleContexts #-}

module SL.TypeChecker.TypeChecker where

import SL.Frontend.Syntax.SLSyntax
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Except
import Control.Monad.State

--estrutura de ados que mantém o estado global e local durante a verificação
data Env = Env
    { envStructs  :: Map.Map String StructDecl
    , envFuncs    :: Map.Map String FuncDecl
    , envVars     :: Map.Map String Type
    , envReturn   :: Maybe Type 
    , envGenerics :: [String]   
    } deriving (Show)

--criação da mônada de verificação
type TypeCheck a = StateT Env (Except String) a

--inicializa um ambiente de verificação vazio
emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty Map.empty Nothing []

--executa as 4 passadas de verificação sobre a AST
checkProgram :: Program -> Either String ()
checkProgram (Program topLevels) = runExcept (evalStateT (checkTopLevels topLevels) emptyEnv)

--ordem das passadas
checkTopLevels :: [TopLevel] -> TypeCheck ()
checkTopLevels tops = do
    mapM_ collectStruct tops
    mapM_ collectFunc tops
    structs <- gets envStructs
    mapM_ checkStructDecl (Map.elems structs)
    mapM_ checkFuncBody tops

--registra o nome e definição de todas as Structs
collectStruct :: TopLevel -> TypeCheck ()
collectStruct (TopStruct s@(StructDecl name _)) = do
    structs <- gets envStructs
    if Map.member name structs
        then throwError $ "Struct '" ++ name ++ "' already defined."
        else do
            modify $ \env -> env { envStructs = Map.insert name s (envStructs env) }
collectStruct _ = return ()

--registra a assinatura das funções
collectFunc :: TopLevel -> TypeCheck ()
collectFunc (TopFunc f@(FuncDecl name _ _ _ _)) = do
    funcs <- gets envFuncs
    if Map.member name funcs
        then throwError $ "Function '" ++ name ++ "' already defined."
        else modify $ \env -> env { envFuncs = Map.insert name f (envFuncs env) }
collectFunc _ = return ()

--verifica se os tipos das strucs são válidos
checkStructDecl :: StructDecl -> TypeCheck ()
checkStructDecl (StructDecl _ fields) = do
    forM_ fields $ \(_, fieldType) -> do
        checkTypeExists fieldType []

--prepara o ambiente local (parametros e retorno) e inicia a verificação do corpo da função
checkFuncBody :: TopLevel -> TypeCheck ()
checkFuncBody (TopFunc (FuncDecl _ generics params retType body)) = do
    oldVars <- gets envVars
    oldRet <- gets envReturn
    oldGens <- gets envGenerics

    modify $ \env -> env { envGenerics = generics }

    forM_ params $ \(pName, pType) -> do
        checkTypeExists pType generics
        addVar pName pType

    checkTypeExists retType generics

    modify $ \env -> env { envReturn = Just retType }

    checkBlock body

    modify $ \env -> env { envVars = oldVars, envReturn = oldRet, envGenerics = oldGens }
checkFuncBody _ = return ()

--inserir variaveis no escopo local
addVar :: String -> Type -> TypeCheck ()
addVar name typ = do
    vars <- gets envVars
    if Map.member name vars
        then throwError $ "Variable '" ++ name ++ "' already declared in this scope."
        else modify $ \env -> env { envVars = Map.insert name typ (envVars env) }

--valida um bloco de código
checkBlock :: Block -> TypeCheck ()
checkBlock stmts = do
    oldVars <- gets envVars
    mapM_ checkStmt stmts
    modify $ \env -> env { envVars = oldVars }

--verificação para cada tipo de comando
checkStmt :: Stmt -> TypeCheck ()
checkStmt (SLet name typ exprMaybe) = do
    gens <- gets envGenerics
    checkTypeExists typ gens
    case exprMaybe of
        Just expr -> do
            exprType <- checkExpr expr
            if isAuto typ
                then addVar name exprType
                else do
                    unless (areTypesCompatible typ exprType) $
                        throwError $ "Type mismatch in declaration of '" ++ name ++ "'. Expected " ++ show typ ++ ", got " ++ show exprType
                    addVar name typ
        Nothing -> do
            if isAuto typ
                then throwError $ "Cannot infer type for variable '" ++ name ++ "' without initialization."
                else addVar name typ

--valida atribuições (garante L-value valido)
checkStmt (SAssign lhs rhs) = do
    lhsType <- checkExpr lhs
    rhsType <- checkExpr rhs
    unless (areTypesCompatible lhsType rhsType) $
        throwError $ "Type mismatch in assignment. Expected " ++ show lhsType ++ ", got " ++ show rhsType
    case lhs of
        EVar _ -> return ()
        EArrayAccess _ _ -> return ()
        EFieldAccess _ _ -> return ()
        _ -> throwError "Invalid l-value in assignment."

--verifica se o tipo da expressão retornada é compatível com a assinatura da função
checkStmt (SReturn exprMaybe) = do
    expectedRet <- gets envReturn
    case (expectedRet, exprMaybe) of
        (Just TyVoid, Nothing) -> return ()
        (Just TyVoid, Just _) -> throwError "Void function should not return a value."
        (Just t, Just expr) -> do
            exprType <- checkExpr expr
            unless (areTypesCompatible t exprType) $
                throwError $ "Return type mismatch. Expected " ++ show t ++ ", got " ++ show exprType
        (Just t, Nothing) -> throwError $ "Function expects return type " ++ show t ++ ", but got nothing."
        (Nothing, _) -> throwError "Return statement outside of function."

--valida estruruas condicionais
checkStmt (SIf cond thenBlock elseBlockMaybe) = do
    condType <- checkExpr cond
    unless (condType == TyBool) $
        throwError $ "If condition must be boolean, got " ++ show condType
    checkBlock thenBlock
    case elseBlockMaybe of
        Just elseBlock -> checkBlock elseBlock
        Nothing -> return ()

--valida o laço de repetição while
checkStmt (SWhile cond body) = do
    condType <- checkExpr cond
    unless (condType == TyBool) $
        throwError $ "While condition must be boolean, got " ++ show condType
    checkBlock body

--valida o laço for
checkStmt (SFor initMaybe cond stepMaybe body) = do
    oldVars <- gets envVars

    case initMaybe of
        Just initStmt -> checkStmt initStmt
        Nothing -> return ()

    condType <- checkExpr cond
    unless (condType == TyBool) $
        throwError $ "For condition must be boolean, got " ++ show condType

    case stepMaybe of
        Just stepStmt -> checkStmt stepStmt
        Nothing -> return ()

    checkBlock body

    modify $ \env -> env { envVars = oldVars }

--verifica expressões de impressão
checkStmt (SPrint expr) = do
    _ <- checkExpr expr
    return ()

--verifica expressões isoladas usadas como comando
checkStmt (SExp expr) = do
    _ <- checkExpr expr
    return ()

--analisa uma expressão e retorna seu tipo ou erro
checkExpr :: Expr -> TypeCheck Type
checkExpr (EInt _) = return TyInt
checkExpr (EFloat _) = return TyFloat
checkExpr (EString _) = return TyString
checkExpr (EBool _) = return TyBool
checkExpr (EVar name) = do
    vars <- gets envVars
    case Map.lookup name vars of
        Just t -> return t
        Nothing -> throwError $ "Variable '" ++ name ++ "' not declared."

--valida operações binárias
checkExpr (EBinOp op l r) = do
    t1 <- checkExpr l
    t2 <- checkExpr r
    checkBinOp op t1 t2

--valida operações unárias
checkExpr (EUnOp op e) = do
    t <- checkExpr e
    checkUnOp op t

--valida chamadas de função
checkExpr (ECall (EVar fName) args) = do
    funcs <- gets envFuncs
    case Map.lookup fName funcs of
        Just (FuncDecl _ _ params retType _) -> do
            if length args /= length params
                then throwError $ "Function '" ++ fName ++ "' expects " ++ show (length params) ++ " arguments, got " ++ show (length args)
                else do
                    argTypes <- mapM checkExpr args
                    let paramTypes = map snd params
                    subst <- foldM (\sub (pt, at) -> case matchTypes pt at sub of
                        Right newSub -> return newSub
                        Left err -> throwError err
                        ) Map.empty (zip paramTypes argTypes)
                    return $ substitute subst retType
        Nothing -> do
            vars <- gets envVars
            case Map.lookup fName vars of
                Just (TyFun paramTypes retType) -> do
                    if length args /= length paramTypes
                        then throwError $ "Function variable '" ++ fName ++ "' expects " ++ show (length paramTypes) ++ " arguments, got " ++ show (length args)
                        else do
                            argTypes <- mapM checkExpr args
                            subst <- foldM (\sub (pt, at) -> case matchTypes pt at sub of
                                Right newSub -> return newSub
                                Left err -> throwError err
                                ) Map.empty (zip paramTypes argTypes)
                            return $ substitute subst retType
                Just _ -> throwError $ "Variable '" ++ fName ++ "' is not a function."
                Nothing -> throwError $ "Function '" ++ fName ++ "' not defined."

--valida chamadas de expressões que resultam em tipos funcionais
checkExpr (ECall expr args) = do
    funcType <- checkExpr expr
    case funcType of
        TyFun paramTypes retType -> do
            if length args /= length paramTypes
                then throwError $ "Function expects " ++ show (length paramTypes) ++ " arguments, got " ++ show (length args)
                else do
                    argTypes <- mapM checkExpr args
                    subst <- foldM (\sub (pt, at) -> case matchTypes pt at sub of
                        Right newSub -> return newSub
                        Left err -> throwError err
                        ) Map.empty (zip paramTypes argTypes)
                    return $ substitute subst retType
        _ -> throwError "Expression is not a function."

--verifica acesso a elementos de array e tipo de indice
checkExpr (EArrayAccess arr index) = do
    arrType <- checkExpr arr
    indexType <- checkExpr index
    unless (indexType == TyInt) $
        throwError "Array index must be integer."
    case arrType of
        TyArray elemType _ -> return elemType
        _ -> throwError "Indexing non-array type."

--verifica acesso a campos de strucs
checkExpr (EFieldAccess obj field) = do
    objType <- checkExpr obj
    case objType of
        TyStruct name -> do
            structs <- gets envStructs
            case Map.lookup name structs of
                Just (StructDecl _ fields) -> do
                    case lookup field fields of
                        Just t -> return t
                        Nothing -> throwError $ "Struct '" ++ name ++ "' does not have field '" ++ field ++ "'."
                Nothing -> throwError $ "Struct '" ++ name ++ "' not defined."
        TyArray _ _ -> do
            if field == "size"
                then return TyInt
                else throwError "Arrays only have 'size' property."
        _ -> throwError "Accessing field of non-struct type."

--valida a alocação de novos objetos
checkExpr (ENew t) = do
    gens <- gets envGenerics
    checkTypeExists t gens
    case t of
        TyArray _ (Just sizeExpr) -> do
            sizeType <- checkExpr sizeExpr
            unless (sizeType == TyInt) $ throwError "Array size must be integer."
        _ -> return ()
    return t

--valida a inicialização literal de uma struct
checkExpr (EStructInit name args) = do
    structs <- gets envStructs
    case Map.lookup name structs of
        Just (StructDecl _ fields) -> do
            if length args /= length fields
                then throwError $ "Struct '" ++ name ++ "' expects " ++ show (length fields) ++ " initializers, got " ++ show (length args)
                else do
                    argTypes <- mapM checkExpr args
                    let fieldTypes = map snd fields
                    zipWithM_ (\expected actual -> unless (areTypesCompatible expected actual) $ throwError $ "Field type mismatch in struct init. Expected " ++ show expected ++ ", got " ++ show actual) fieldTypes argTypes
                    return (TyStruct name)
        Nothing -> throwError $ "Struct '" ++ name ++ "' not defined."

--valida literais de array
checkExpr (EArrayLit exprs) = do
    if null exprs
        then throwError "Empty array literal not supported (cannot infer type)."
        else do
            types <- mapM checkExpr exprs
            let firstType = head types
            unless (all (areTypesCompatible firstType) types) $
                throwError "Array literal elements must have compatible types."
            return (TyArray firstType (Just (EInt (length exprs))))

--direciona operções binárias para suas validações semânticas especificas
checkBinOp :: BinOp -> Type -> Type -> TypeCheck Type
checkBinOp op t1 t2 = case op of
    Add -> checkMathOp t1 t2
    Sub -> checkMathOp t1 t2
    Mul -> checkMathOp t1 t2
    Div -> checkMathOp t1 t2
    And -> checkBoolOp t1 t2
    Or  -> checkBoolOp t1 t2
    Gt  -> checkCompOp t1 t2
    Lt  -> checkCompOp t1 t2
    Ge  -> checkCompOp t1 t2
    Le  -> checkCompOp t1 t2
    Eq  -> checkEqOp t1 t2
    Ne  -> checkEqOp t1 t2

--regras para operções matemáticas
checkMathOp :: Type -> Type -> TypeCheck Type
checkMathOp TyInt TyInt = return TyInt
checkMathOp TyFloat TyFloat = return TyFloat
checkMathOp TyInt TyFloat = return TyFloat
checkMathOp TyFloat TyInt = return TyFloat
checkMathOp TyString TyString = return TyString
checkMathOp t1 t2 = throwError $ "Invalid operand types for math operation: " ++ show t1 ++ " and " ++ show t2

--regras para operções lógicas
checkBoolOp :: Type -> Type -> TypeCheck Type
checkBoolOp TyBool TyBool = return TyBool
checkBoolOp t1 t2 = throwError $ "Invalid operand types for boolean operation: " ++ show t1 ++ " and " ++ show t2

--regras para comparação
checkCompOp :: Type -> Type -> TypeCheck Type
checkCompOp TyInt TyInt = return TyBool
checkCompOp TyFloat TyFloat = return TyBool
checkCompOp TyInt TyFloat = return TyBool
checkCompOp TyFloat TyInt = return TyBool
checkCompOp t1 t2 = throwError $ "Invalid operand types for comparison: " ++ show t1 ++ " and " ++ show t2

--regras para igualdade
checkEqOp :: Type -> Type -> TypeCheck Type
checkEqOp t1 t2 | areTypesCompatible t1 t2 = return TyBool
checkEqOp t1 t2 = throwError $ "Invalid operand types for equality check: " ++ show t1 ++ " and " ++ show t2

--regras para operadoes unários
checkUnOp :: UnOp -> Type -> TypeCheck Type
checkUnOp Not TyBool = return TyBool
checkUnOp Neg TyInt = return TyInt
checkUnOp Neg TyFloat = return TyFloat
checkUnOp _ t = throwError $ "Invalid operand type for unary operation: " ++ show t

--predicato para identificar o tipo especial auto usado em inverência
isAuto :: Type -> Bool
isAuto (TyVar "auto") = True
isAuto _ = False

--define se um tipo pode ser atribuído ou usado onde outro tipo é esperado
areTypesCompatible :: Type -> Type -> Bool
areTypesCompatible t1 t2 | t1 == t2 = True
areTypesCompatible TyFloat TyInt = True 
areTypesCompatible TyInt TyFloat = False 
areTypesCompatible (TyArray t1 _) (TyArray t2 _) = areTypesCompatible t1 t2
areTypesCompatible (TyVar _) _ = True 
areTypesCompatible _ (TyVar _) = True
areTypesCompatible _ _ = False

--valida se um tipo mencionado no código foi devidamente declarado no ambiente global
checkTypeExists :: Type -> [String] -> TypeCheck ()
checkTypeExists (TyStruct name) gens
    | name `elem` gens = return () 
    | otherwise = do
        structs <- gets envStructs
        unless (Map.member name structs) $ throwError $ "Undefined struct type: " ++ name
checkTypeExists (TyArray t sizeMaybe) gens = do
    checkTypeExists t gens
    case sizeMaybe of
        Just expr -> do
            tSize <- checkExpr expr
            unless (tSize == TyInt) $ throwError "Array size must be an integer."
        Nothing -> return ()
checkTypeExists (TyVar name) gens
    | name == "auto" = return ()
    | name `elem` gens = return ()
    | otherwise = return () 
checkTypeExists _ _ = return ()

--unificação simples de tipos para lidar com variáveis de tipo (Generics)
matchTypes :: Type -> Type -> Map.Map String Type -> Either String (Map.Map String Type)
matchTypes (TyVar v) argType subst
    | v == "auto" = Right subst
    | otherwise = case Map.lookup v subst of
        Just existing -> if areTypesCompatible existing argType
            then Right subst
            else Left $ "Type variable " ++ v ++ " mismatch: expected " ++ show existing ++ ", got " ++ show argType
        Nothing -> Right (Map.insert v argType subst)
matchTypes (TyArray t1 _) (TyArray t2 _) subst = matchTypes t1 t2 subst
matchTypes t1 t2 subst
    | areTypesCompatible t1 t2 = Right subst
    | otherwise = Left $ "Type mismatch: expected " ++ show t1 ++ ", got " ++ show t2

--aplica as substituições de tipos genéricos em uma definição de tipo
substitute :: Map.Map String Type -> Type -> Type
substitute subst (TyVar v) = Map.findWithDefault (TyVar v) v subst
substitute subst (TyArray t sz) = TyArray (substitute subst t) sz
substitute subst (TyFun args ret) = TyFun (map (substitute subst) args) (substitute subst ret)
substitute _ t = t
