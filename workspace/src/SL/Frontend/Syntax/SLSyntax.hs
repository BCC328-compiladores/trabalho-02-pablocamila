module SL.Frontend.Syntax.SLSyntax where

-- Tipos do Programa foram separados hierarquicamente:
    -- Programa contem niveis acima (toplevels)

data Program = Program [TopLevel]
    deriving (Show,Eq)

-- Niveis Top podem ser ou funcoes ou structs e suas declaracoes
data TopLevel
    = TopFunc FuncDecl
    | TopStruct StructDecl
    deriving (Show, Eq)

-- Declaracoes usam `Records` de Haskell
-- Declaracao de structs contem nome da struct
-- e campos de struct (nome identificador e tipo)
data StructDecl = StructDecl {
    structName :: String,
    structFields :: [(String, Type)]
} deriving (Show, Eq)

-- Declaracao de Funcao contem nome da funcao, generico
-- parametros, tipo do retorno e o corpo da funcao
data FuncDecl = FuncDecl{
    funcName :: String,
    funcGenerics :: [String], -- Genericos tipo (forall a . func ...)
    funcParams :: [(String, Type)],
    funcRetType :: Type,
    funcBody :: Block -- tipo bloco definido abaixo
} deriving (Show, Eq)

type Block = [Stmt] -- Lista de stataments (comandos)

-- Stataments/Comandos da linguagem SL e as expressoes
data Stmt
    = SLet String Type (Maybe Expr) -- Maybe pois pode criar variavel sem incializar elas
    | SAssign Expr Expr             -- x = y
    | SReturn (Maybe Expr)          -- return x; Maybe para retorno de nada (return )
    | SIf Expr Block (Maybe Block)  -- if ... else. Maybe Block e referente ao Else que pode ou nao existir
    | SWhile Expr Block             -- while
    | SFor (Maybe Stmt) (Expr) (Maybe Stmt) Block -- For pode contem 3 condicoes (i=0, i<10, i++) ou apenas uma de cada ou nenhuma (Loop Infinito)
    | SPrint Expr                   -- print()
    | SExp Expr                     -- Somente a expressao
    deriving (Show, Eq)

data Expr
    = EInt Int | EFloat Float | EString String | EBool Bool -- Tipos basicos
    | EVar String
    | EBinOp BinOp Expr Expr            -- Usa BinOp para qualquer operacao que pode ser binaria (add, sub, mult), ai ja define pra todas
    | EUnOp UnOp Expr                   -- Usa UnOp para operacao Unaria que so utiliza uma expressao
    | ECall Expr [Expr]
    | EArrayAccess Expr Expr
    | EFieldAccess Expr String          -- obj.campo
    | ENew Type                         -- new T e tambem nre T[Size]
    | EStructInit String [Expr]         -- Inicia nova struct ex: Person{...}
    | EArrayLit [Expr]                  -- vetor de literais, exp: [1, 2, 3]
    deriving (Show, Eq)

-- Tipos dos dados
data Type
    = TyInt | TyFloat | TyString | TyBool | TyVoid
    | TyStruct String           -- struct contem seu nome
    | TyVar String              -- Tipo generico 
    | TyArray Type (Maybe Expr) -- Vetor com seu tipo e talvez []
    | TyFun [Type] Type         -- Tipo para funcoes de alta ordem 
    deriving (Show, Eq)

-- Operadores Binarios
data BinOp
    = Add | Sub | Mul | Div
    | And | Or
    | Gt | Lt | Ge | Le | Eq | Ne
    deriving (Show, Eq)

-- Operadores Unarios
data UnOp
    = Not | Neg
    deriving (Show, Eq)