{-# LANGUAGE FlexibleContexts #-}

module SL.Interpreter.Interpreter where

import SL.Frontend.Syntax.SLSyntax
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Array.IO
import Data.Maybe (fromMaybe)

-- Representacao dos valores em tempo de execucao
data Value
    = VInt Int
    | VFloat Float
    | VString String
    | VBool Bool
    | VVoid
    | VNull
    | VArray (IORef (IOArray Int Value))        -- Referencia mutavel para array
    | VStruct String (IORef (Map.Map String Value)) -- Referencia mutavel para struct

instance Show Value where
    show (VInt i) = show i
    show (VFloat f) = show f
    show (VString s) = s
    show (VBool b) = show b
    show VVoid = "void"
    show VNull = "null"
    show (VArray _) = "[Array]"
    show (VStruct name _) = "Struct " ++ name

-- Memoria e estado global do interpretador
data RuntimeEnv = RuntimeEnv
    { envVars    :: Map.Map String (IORef Value) -- Mapa de variaveis para referencias de memoria
    , envFuncs   :: Map.Map String FuncDecl      -- Definicoes de funcoes
    , envStructs :: Map.Map String StructDecl    -- Definicoes de structs
    , envRetVal  :: Maybe Value                  -- Armazena valor de retorno temporario
    }

-- Monada que combina Estado (memoria) e IO (efeitos colaterais)
type Interp a = StateT RuntimeEnv IO a

emptyEnv :: RuntimeEnv
emptyEnv = RuntimeEnv Map.empty Map.empty Map.empty Nothing

-- Prepara o ambiente com funcoes/structs e inicia a execucao pela main
evalProgram :: Program -> IO ()
evalProgram (Program topLevels) = do
    let (funcs, structs) = foldl collect (Map.empty, Map.empty) topLevels
    let env = emptyEnv { envFuncs = funcs, envStructs = structs }
    evalStateT (runMain) env
  where
    collect (fs, ss) (TopFunc f) = (Map.insert (funcName f) f fs, ss)
    collect (fs, ss) (TopStruct s) = (fs, Map.insert (structName s) s ss)

    runMain :: Interp ()
    runMain = do
        funcs <- gets envFuncs
        case Map.lookup "main" funcs of
            Just mainFunc -> do
                _ <- evalFuncCall mainFunc []
                return ()
            Nothing -> liftIO $ putStrLn "Erro: funcao main nao encontrada."

-- Gerencia escopo de funcao: cria novas referencias para argumentos e executa corpo
evalFuncCall :: FuncDecl -> [Value] -> Interp Value
evalFuncCall (FuncDecl _ _ params _ body) args = do
    globals <- get

    -- Aloca nova memoria para os parametros (passagem por valor)
    localVars <- liftIO $ foldM (\m ((pName, _), val) -> do
        ref <- newIORef val
        return $ Map.insert pName ref m
        ) Map.empty (zip params args)

    -- Troca para o escopo local
    put $ globals { envVars = localVars, envRetVal = Nothing }

    executeBlock body

    ret <- gets envRetVal

    -- Restaura o escopo anterior
    put globals

    return $ fromMaybe VVoid ret

-- Executa uma sequencia de comandos
executeBlock :: Block -> Interp ()
executeBlock stmts = do
    oldVars <- gets envVars

    mapM_ execStmt stmts

    -- Se nao houve return, restaura variaveis locais
    ret <- gets envRetVal
    case ret of
        Just _ -> return ()
        Nothing -> modify $ \env -> env { envVars = oldVars }

-- Verifica se ja houve retorno antes de executar o proximo comando
execStmt :: Stmt -> Interp ()
execStmt stmt = do
    ret <- gets envRetVal
    case ret of
        Just _ -> return ()
        Nothing -> realExecStmt stmt

-- Executa o comando alterando a memoria real
realExecStmt :: Stmt -> Interp ()
realExecStmt (SLet name typ exprMaybe) = do
    val <- case exprMaybe of
        Just expr -> evalExpr expr
        Nothing -> defaultValue typ

    -- Cria nova referencia na memoria e associa ao nome
    vars <- gets envVars
    ref <- liftIO $ newIORef val
    modify $ \env -> env { envVars = Map.insert name ref vars }

realExecStmt (SAssign lhs rhs) = do
    val <- evalExpr rhs
    case lhs of
        -- Atualiza variavel existente na memoria
        EVar name -> do
            vars <- gets envVars
            case Map.lookup name vars of
                Just ref -> liftIO $ writeIORef ref val
                Nothing -> liftIO $ putStrLn $ "Erro em Tempo de Execucao: Variavel '" ++ name ++ "' nao encontrada."
        -- Atualiza posicao de array na memoria
        EArrayAccess arrExpr indexExpr -> do
            arrVal <- evalExpr arrExpr
            indexVal <- evalExpr indexExpr
            case (arrVal, indexVal) of
                (VArray ref, VInt i) -> do
                    arr <- liftIO $ readIORef ref
                    bounds <- liftIO $ getBounds arr
                    if i >= fst bounds && i <= snd bounds
                        then liftIO $ writeArray arr i val
                        else liftIO $ putStrLn "Erro em Tempo de Execucao: Indice do array fora dos limites."
                _ -> liftIO $ putStrLn "Erro em Tempo de Execucao: Atribuicao de array invalida."
        -- Atualiza campo de struct na memoria
        EFieldAccess objExpr field -> do
            objVal <- evalExpr objExpr
            case objVal of
                VStruct _ fieldsRef -> do
                    fields <- liftIO $ readIORef fieldsRef
                    liftIO $ writeIORef fieldsRef (Map.insert field val fields)
                _ -> liftIO $ putStrLn "Erro em Tempo de Execucao: Atribuicao de campo invalida."
        _ -> liftIO $ putStrLn "Erro em Tempo de Execucao: Alvo de atribuicao invalido."

realExecStmt (SReturn exprMaybe) = do
    val <- case exprMaybe of
        Just expr -> evalExpr expr
        Nothing -> return VVoid
    -- Define o valor de retorno no estado
    modify $ \env -> env { envRetVal = Just val }

realExecStmt (SIf cond thenBlock elseBlockMaybe) = do
    condVal <- evalExpr cond
    case condVal of
        VBool True -> executeBlock thenBlock
        VBool False -> case elseBlockMaybe of
            Just elseBlock -> executeBlock elseBlock
            Nothing -> return ()
        _ -> liftIO $ putStrLn "Erro em Tempo de Execucao: Condicao do If nao e booleana."

realExecStmt (SWhile cond body) = loop
  where
    loop = do
        condVal <- evalExpr cond
        case condVal of
            VBool True -> do
                executeBlock body
                ret <- gets envRetVal
                case ret of
                    Just _ -> return () -- Interrompe se houver return
                    Nothing -> loop
            VBool False -> return ()
            _ -> liftIO $ putStrLn "Erro em Tempo de Execucao: Condicao do While nao e booleana."

realExecStmt (SFor initMaybe cond stepMaybe body) = do
    oldVars <- gets envVars

    case initMaybe of
        Just s -> execStmt s
        Nothing -> return ()

    loop

    modify $ \env -> env { envVars = oldVars }
  where
    loop = do
        condVal <- evalExpr cond
        case condVal of
            VBool True -> do
                executeBlock body
                ret <- gets envRetVal
                case ret of
                    Just _ -> return ()
                    Nothing -> do
                        case stepMaybe of
                            Just s -> execStmt s
                            Nothing -> return ()
                        loop
            VBool False -> return ()
            _ -> liftIO $ putStrLn "Erro em Tempo de Execucao: Condicao do For nao e booleana."

realExecStmt (SPrint expr) = do
    val <- evalExpr expr
    case val of
        VString s -> liftIO $ putStrLn s
        _ -> liftIO $ print val

realExecStmt (SExp expr) = do
    _ <- evalExpr expr
    return ()

-- Gera valor inicial padrao para variaveis sem atribuicao
defaultValue :: Type -> Interp Value
defaultValue TyInt = return $ VInt 0
defaultValue TyFloat = return $ VFloat 0.0
defaultValue TyString = return $ VString ""
defaultValue TyBool = return $ VBool False
defaultValue TyVoid = return VVoid
defaultValue (TyArray elemType (Just sizeExpr)) = do
    -- Aloca memoria para array novo
    sizeVal <- evalExpr sizeExpr
    case sizeVal of
        VInt size -> do
            vals <- replicateM size (defaultValue elemType)
            arr <- liftIO $ newListArray (0, size - 1) vals
            ref <- liftIO $ newIORef arr
            return $ VArray ref
        _ -> return VNull
defaultValue (TyArray _ Nothing) = return VNull
defaultValue (TyStruct _) = return VNull
defaultValue _ = return VNull

-- Calcula o valor de uma expressao
evalExpr :: Expr -> Interp Value
evalExpr (EInt i) = return $ VInt i
evalExpr (EFloat f) = return $ VFloat f
evalExpr (EString s) = return $ VString s
evalExpr (EBool b) = return $ VBool b
evalExpr (EVar name) = do
    vars <- gets envVars
    case Map.lookup name vars of
        Just ref -> liftIO $ readIORef ref -- Le valor da memoria
        Nothing -> do
            liftIO $ putStrLn $ "Erro em Tempo de Execucao: Variavel '" ++ name ++ "' nao encontrada."
            return VNull

evalExpr (EBinOp op l r) = do
    v1 <- evalExpr l
    v2 <- evalExpr r
    applyBinOp op v1 v2

evalExpr (EUnOp op e) = do
    v <- evalExpr e
    applyUnOp op v

evalExpr (ECall (EVar fName) args) = do
    argVals <- mapM evalExpr args
    funcs <- gets envFuncs
    case Map.lookup fName funcs of
        Just func -> evalFuncCall func argVals
        Nothing -> do
            liftIO $ putStrLn $ "Erro em Tempo de Execucao: Funcao '" ++ fName ++ "' nao encontrada."
            return VNull
evalExpr (ECall _ _) = return VNull

evalExpr (EArrayAccess arrExpr indexExpr) = do
    arrVal <- evalExpr arrExpr
    indexVal <- evalExpr indexExpr
    case (arrVal, indexVal) of
        (VArray ref, VInt i) -> do
            arr <- liftIO $ readIORef ref
            bounds <- liftIO $ getBounds arr
            if i >= fst bounds && i <= snd bounds
                then liftIO $ readArray arr i -- Leitura com bounds checking
                else do
                    liftIO $ putStrLn "Erro em Tempo de Execucao: Indice do array fora dos limites."
                    return VNull
        _ -> return VNull

evalExpr (EFieldAccess objExpr field) = do
    objVal <- evalExpr objExpr
    case objVal of
        VStruct _ fieldsRef -> do
            fields <- liftIO $ readIORef fieldsRef
            case Map.lookup field fields of
                Just val -> return val
                Nothing -> do
                    liftIO $ putStrLn $ "Erro em Tempo de Execucao: Campo '" ++ field ++ "' nao encontrado."
                    return VNull
        VArray ref -> do
            if field == "size"
                then do
                    arr <- liftIO $ readIORef ref
                    bounds <- liftIO $ getBounds arr
                    return $ VInt (snd bounds - fst bounds + 1)
                else return VNull
        _ -> return VNull

evalExpr (ENew t) = do
    case t of
        TyArray elemType (Just sizeExpr) -> do
            sizeVal <- evalExpr sizeExpr
            case sizeVal of
                VInt size -> do
                    vals <- replicateM size (defaultValue elemType)
                    arr <- liftIO $ newListArray (0, size - 1) vals
                    ref <- liftIO $ newIORef arr
                    return $ VArray ref
                _ -> return VNull
        TyStruct name -> do
            structs <- gets envStructs
            case Map.lookup name structs of
                Just (StructDecl _ fields) -> do
                    fieldVals <- mapM (\(_, fType) -> defaultValue fType) fields
                    let fieldMap = Map.fromList $ zip (map fst fields) fieldVals
                    ref <- liftIO $ newIORef fieldMap
                    return $ VStruct name ref
                Nothing -> return VNull
        _ -> return VNull

evalExpr (EStructInit name args) = do
    argVals <- mapM evalExpr args
    structs <- gets envStructs
    case Map.lookup name structs of
        Just (StructDecl _ fields) -> do
            let fieldMap = Map.fromList $ zip (map fst fields) argVals
            ref <- liftIO $ newIORef fieldMap
            return $ VStruct name ref
        Nothing -> return VNull

evalExpr (EArrayLit exprs) = do
    vals <- mapM evalExpr exprs
    let size = length vals
    arr <- liftIO $ newListArray (0, size - 1) vals
    ref <- liftIO $ newIORef arr
    return $ VArray ref

-- Realiza a operacao matematica ou logica entre valores
applyBinOp :: BinOp -> Value -> Value -> Interp Value
applyBinOp Add (VInt a) (VInt b) = return $ VInt (a + b)
applyBinOp Add (VFloat a) (VFloat b) = return $ VFloat (a + b)
applyBinOp Add (VInt a) (VFloat b) = return $ VFloat (fromIntegral a + b)
applyBinOp Add (VFloat a) (VInt b) = return $ VFloat (a + fromIntegral b)
applyBinOp Add (VString a) (VString b) = return $ VString (a ++ b)
applyBinOp Sub (VInt a) (VInt b) = return $ VInt (a - b)
applyBinOp Sub (VFloat a) (VFloat b) = return $ VFloat (a - b)
applyBinOp Mul (VInt a) (VInt b) = return $ VInt (a * b)
applyBinOp Mul (VFloat a) (VFloat b) = return $ VFloat (a * b)
applyBinOp Div (VInt a) (VInt b) = return $ VInt (a `div` b)
applyBinOp Div (VFloat a) (VFloat b) = return $ VFloat (a / b)

applyBinOp And (VBool a) (VBool b) = return $ VBool (a && b)
applyBinOp Or (VBool a) (VBool b) = return $ VBool (a || b)

applyBinOp Gt (VInt a) (VInt b) = return $ VBool (a > b)
applyBinOp Gt (VFloat a) (VFloat b) = return $ VBool (a > b)
applyBinOp Gt (VInt a) (VFloat b) = return $ VBool (fromIntegral a > b)
applyBinOp Gt (VFloat a) (VInt b) = return $ VBool (a > fromIntegral b)

applyBinOp Lt (VInt a) (VInt b) = return $ VBool (a < b)
applyBinOp Lt (VFloat a) (VFloat b) = return $ VBool (a < b)
applyBinOp Lt (VInt a) (VFloat b) = return $ VBool (fromIntegral a < b)
applyBinOp Lt (VFloat a) (VInt b) = return $ VBool (a < fromIntegral b)

applyBinOp Le (VInt a) (VInt b) = return $ VBool (a <= b)
applyBinOp Le (VFloat a) (VFloat b) = return $ VBool (a <= b)
applyBinOp Le (VInt a) (VFloat b) = return $ VBool (fromIntegral a <= b)
applyBinOp Le (VFloat a) (VInt b) = return $ VBool (a <= fromIntegral b)

applyBinOp Ge (VInt a) (VInt b) = return $ VBool (a >= b)
applyBinOp Ge (VFloat a) (VFloat b) = return $ VBool (a >= b)
applyBinOp Ge (VInt a) (VFloat b) = return $ VBool (fromIntegral a >= b)
applyBinOp Ge (VFloat a) (VInt b) = return $ VBool (a >= fromIntegral b)

applyBinOp Eq (VInt a) (VInt b) = return $ VBool (a == b)
applyBinOp Eq (VFloat a) (VFloat b) = return $ VBool (a == b)
applyBinOp Eq (VInt a) (VFloat b) = return $ VBool (fromIntegral a == b)
applyBinOp Eq (VFloat a) (VInt b) = return $ VBool (a == fromIntegral b)
applyBinOp Eq (VString a) (VString b) = return $ VBool (a == b)

applyBinOp Ne (VInt a) (VInt b) = return $ VBool (a /= b)
applyBinOp Ne (VFloat a) (VFloat b) = return $ VBool (a /= b)
applyBinOp Ne (VInt a) (VFloat b) = return $ VBool (fromIntegral a /= b)
applyBinOp Ne (VFloat a) (VInt b) = return $ VBool (a /= fromIntegral b)
applyBinOp Ne (VString a) (VString b) = return $ VBool (a /= b)

applyBinOp _ _ _ = return VNull

applyUnOp :: UnOp -> Value -> Interp Value
applyUnOp Neg (VInt a) = return $ VInt (-a)
applyUnOp Neg (VFloat a) = return $ VFloat (-a)
applyUnOp Not (VBool a) = return $ VBool (not a)
applyUnOp _ _ = return VNull