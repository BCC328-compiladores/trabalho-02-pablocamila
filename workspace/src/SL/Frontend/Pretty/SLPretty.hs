module SL.Frontend.Pretty.SLPretty where

-- Importa a biblioteca para Pretty-Printing de documentos formatados.
import Text.PrettyPrint hiding ((<>)) 
-- Importa a estrutura da Abstract Syntax Tree (AST) da linguagem SL.
import SL.Frontend.Syntax.SLSyntax

-- Main entry point
-- ppProgram: Formata o programa inteiro (uma lista de TopLevels).
-- Usa vcat para concatenar as declarações verticalmente, uma por linha.
ppProgram :: Program -> Doc
ppProgram (Program topLevels) = vcat (map ppTopLevel topLevels)

-- ppTopLevel: Formata declarações de nível superior (funções ou structs).
ppTopLevel :: TopLevel -> Doc
ppTopLevel (TopFunc func) = ppFunc func
ppTopLevel (TopStruct struct) = ppStruct struct

-- ppStruct: Formata a declaração de uma estrutura (struct).
-- Formato: struct <name> { ... campos ... }
ppStruct :: StructDecl -> Doc
ppStruct (StructDecl name fields) =
  text "struct" <+> text name <+> lbrace $+$
  -- Recua os campos em 4 espaços.
  nest 4 (vcat (map ppField fields)) $+$
  rbrace

-- ppField: Formata um campo dentro de uma struct.
-- Formato: <name> : <type>;
ppField :: (String, Type) -> Doc
ppField (name, typ) = text name <+> colon <+> (ppType typ <> semi)

-- ppFunc: Formata a declaração de uma função.
-- Formato: [forall ...] func <name>(<params>): <retType> { ... body ... }
ppFunc :: FuncDecl -> Doc
ppFunc (FuncDecl name generics params retType body) =
  -- Formata os genéricos (se houver)
  ppGenerics generics <+> text "func" <+> (text name <> parens (ppParams params)) <+> ppRetType retType <+> lbrace $+$
  -- Recua o bloco de código (body) em 4 espaços.
  nest 4 (ppBlock body) $+$
  rbrace

-- ppGenerics: Formata a cláusula 'forall' para genéricos.
-- Se vazia, retorna 'empty'. Ex: forall T U .
ppGenerics :: [String] -> Doc
ppGenerics [] = empty
ppGenerics vars = text "forall" <+> hsep (map text vars) <+> char '.'

-- ppParams: Formata a lista de parâmetros de uma função, separando-os por vírgula.
ppParams :: [(String, Type)] -> Doc
ppParams [] = empty
ppParams params = hcat (punctuate (comma <+> space) (map ppParam params))

-- ppParam: Formata um único parâmetro. Ex: x: int
ppParam :: (String, Type) -> Doc
ppParam (name, typ) = text name <> ppTypeAnnot typ

-- ppTypeAnnot: Formata a anotação de tipo (a parte ': <type>').
-- Omite a anotação se for o tipo "auto" (inferência de tipo).
ppTypeAnnot :: Type -> Doc
ppTypeAnnot (TyVar "auto") = empty
ppTypeAnnot t = (space <> colon) <+> ppType t

-- ppRetType: Formata o tipo de retorno da função.
-- Omite se for TyVoid. Ex: : int
ppRetType :: Type -> Doc
ppRetType TyVoid = empty
ppRetType typ = colon <+> ppType typ

-- ppType: Formata os diferentes tipos de dados.
ppType :: Type -> Doc
ppType TyInt = text "int"
ppType TyFloat = text "float"
ppType TyString = text "string"
ppType TyBool = text "bool"
ppType TyVoid = text "void"
-- Array de tamanho indefinido: T[]
ppType (TyArray t Nothing) = ppType t <> text "[]"
-- Array com tamanho explícito: T[Expr]
ppType (TyArray t (Just e)) = ppType t <> brackets (ppExpr e)
-- Tipos Struct
ppType (TyStruct s) = text s
-- Variáveis de Tipo (genéricos)
ppType (TyVar s) = text s
-- Tipo Função: (Arg1, Arg2, ...) -> Retorno
ppType (TyFun args ret) = parens (hcat (punctuate (comma <+> space) (map ppType args))) <+> text "->" <+> ppType ret

-- ppBlock: Formata um bloco de declarações (o corpo de uma função ou loop).
-- Coloca cada declaração em uma nova linha (vcat).
ppBlock :: Block -> Doc
ppBlock stmts = vcat (map ppStmt stmts)

-- ppStmt: Formata uma única declaração (statement).
ppStmt :: Stmt -> Doc
-- SLet: Declaração de variável. Ex: let x: int = 10;
ppStmt (SLet name typ expr) =
  text "let" <+> (text name <> ppTypeAnnot typ) <+> (ppInit expr <> semi)
  where
    ppInit Nothing = empty
    ppInit (Just e) = (space <> equals) <+> ppExpr e -- Adiciona '= <expr>' se houver inicialização.
-- SAssign: Atribuição. Ex: x = y + 1;
ppStmt (SAssign lhs rhs) = ppExpr lhs <+> equals <+> (ppExpr rhs <> semi)
-- SReturn: Retorno. Ex: return x; ou return;
ppStmt (SReturn expr) = text "return" <+> (maybe empty ppExpr expr <> semi)
-- SIf: Condicional. Ex: if (cond) { ... } else { ... }
ppStmt (SIf cond thenBlock elseBlock) =
  text "if" <+> parens (ppExpr cond) <+> lbrace $+$
  nest 4 (ppBlock thenBlock) $+$
  rbrace <+> ppElse elseBlock
  where
    -- ppElse: Lida com o bloco 'else' opcional.
    ppElse Nothing = empty
    ppElse (Just blk) = text "else" <+> lbrace $+$ nest 4 (ppBlock blk) $+$ rbrace
-- SWhile: Loop while. Ex: while (cond) { ... }
ppStmt (SWhile cond body) =
  text "while" <+> parens (ppExpr cond) <+> lbrace $+$
  nest 4 (ppBlock body) $+$
  rbrace
-- SFor: Loop for. Ex: for (init; cond; step) { ... }
ppStmt (SFor forInit cond step body) =
  text "for" <+> parens ((ppForInit forInit <> semi) <+> (ppForCond cond <> semi) <+> ppForStep step) <+> lbrace $+$
  nest 4 (ppBlock body) $+$
  rbrace
-- SPrint: Função de impressão. Ex: print(x);
ppStmt (SPrint expr) = text "print" <> parens (ppExpr expr) <> semi
-- SExp: Expressão usada como statement (ex: chamada de função). Ex: foo();
ppStmt (SExp expr) = ppExpr expr <> semi

-- Funções auxiliares para formatar as partes do loop 'for' (init, cond, step).

ppForInit :: Maybe Stmt -> Doc
ppForInit Nothing = empty
ppForInit (Just s) = case s of
  -- Permite let na inicialização: let x: int = 0
  SLet n t e -> text "let" <+> text n <+> (if t == TyVar "auto" then empty else colon <+> ppType t) <+> (case e of Just x -> equals <+> ppExpr x; Nothing -> empty)
  -- Permite atribuição: x = 0
  SAssign l r -> ppExpr l <+> equals <+> ppExpr r
  -- Permite expressão (ex: chamada de função): initFn()
  SExp e -> ppExpr e
  _ -> empty

ppForCond :: Expr -> Doc
ppForCond e = ppExpr e

ppForStep :: Maybe Stmt -> Doc
ppForStep Nothing = empty
ppForStep (Just s) = case s of
  -- Permite atribuição no passo: i = i + 1
  SAssign l r -> ppExpr l <+> equals <+> ppExpr r
  -- Permite expressão no passo: i++ (se SExp for capaz de representar isso)
  SExp e -> ppExpr e
  _ -> empty

-- ppExpr: Formata uma expressão.
ppExpr :: Expr -> Doc
ppExpr (EInt i) = int i
ppExpr (EFloat f) = float f
ppExpr (EString s) = doubleQuotes (text s)
ppExpr (EBool b) = text (if b then "true" else "false")
ppExpr (EVar s) = text s
ppExpr (EBinOp op l r) = ppExpr l <+> ppBinOp op <+> ppExpr r
ppExpr (EUnOp op e) = ppUnOp op <> ppExpr e
ppExpr (ECall f args) = ppExpr f <> parens (hcat (punctuate (comma <+> space) (map ppExpr args)))
ppExpr (EArrayAccess arr idx) = ppExpr arr <> brackets (ppExpr idx)
ppExpr (EFieldAccess obj field) = ppExpr obj <> char '.' <> text field
ppExpr (ENew typ) = text "new" <+> ppType typ
ppExpr (EStructInit name args) = text name <> lbrace <> hcat (punctuate (comma <+> space) (map ppExpr args)) <> rbrace
ppExpr (EArrayLit exprs) = brackets (hcat (punctuate (comma <+> space) (map ppExpr exprs)))

-- ppBinOp: Mapeia os operadores binários para seus símbolos.
ppBinOp :: BinOp -> Doc
ppBinOp Add = char '+'
ppBinOp Sub = char '-'
ppBinOp Mul = char '*'
ppBinOp Div = char '/'
ppBinOp And = text "&&"
ppBinOp Or = text "||"
ppBinOp Gt = char '>'
ppBinOp Lt = char '<'
ppBinOp Ge = text ">="
ppBinOp Le = text "<="
ppBinOp Eq = text "=="
ppBinOp Ne = text "!="

-- ppUnOp: Mapeia os operadores unários para seus símbolos.
ppUnOp :: UnOp -> Doc
ppUnOp Not = char '!'
ppUnOp Neg = char '-'