module Main where

-- Importacoes das bibliotecas necessarias
import Options.Applicative
import SL.Frontend.Lexer.SLLexer (lexer)
import SL.Frontend.Parser.SLParser (slParser)
import SL.Frontend.Pretty.SLPretty (ppProgram)
import SL.TypeChecker.TypeChecker (checkProgram)
import Text.PrettyPrint (render)
import System.Exit (exitFailure)
import Data.Tree (drawTree, Tree(..))
import SL.Frontend.Syntax.SLSyntax
import System.IO (withFile, hSetEncoding, utf8, hGetContents, IOMode(..), stdout)
import Control.Exception (evaluate)

-- Estrutura de dados para armazenar as opcoes de linha de comando
data Options = Options
  { optLexer :: Bool    -- Flag para rodar apenas o lexer
  , optParser :: Bool   -- Flag para rodar o parser e mostrar a AST
  , optPretty :: Bool   -- Flag para rodar o pretty printer
  , optCheck :: Bool    -- Flag para rodar o type checker
  , optInput :: FilePath -- Caminho do arquivo de entrada
  }

-- Definicao do parser de argumentos da linha de comando
optionsParser :: Parser Options
optionsParser = Options
  <$> switch (long "lexer" <> help "Run lexer")
  <*> switch (long "parser" <> help "Run parser")
  <*> switch (long "pretty" <> help "Run pretty printer")
  <*> switch (long "check" <> help "Run type checker")
  <*> strArgument (metavar "FILE" <> help "Input file")

-- Ponto de entrada do programa
main :: IO ()
main = execParser optsDef >>= run

-- Funcao principal de execucao
run :: Options -> IO ()
run opts = do
  hSetEncoding stdout utf8 -- Configura a saida padrao para UTF-8 (corrige erro de caracteres especiais)
  -- Leitura do arquivo de entrada
  content <- withFile (optInput opts) ReadMode $ \h -> do
    hSetEncoding h utf8 -- Garante que o arquivo seja lido como UTF-8
    s <- hGetContents h
    _ <- evaluate (length s) -- Forca a leitura completa do arquivo imediatamente
    return s
  -- Logica de selecao baseada nas flags
  if optLexer opts
    then do
      -- Executa o lexer e imprime os tokens
      case lexer content of
        Left err -> putStrLn err >> exitFailure
        Right tokens -> mapM_ print tokens
    else if optParser opts
      then do
        -- Executa o parser e desenha a arvore de sintaxe (AST)
        case slParser content of
          Left err -> putStrLn err >> exitFailure
          Right ast -> putStrLn (drawTree (astToTree ast))
    else if optPretty opts
      then do
        -- Executa o parser e imprime o codigo formatado (pretty print)
        case slParser content of
          Left err -> putStrLn err >> exitFailure
          Right ast -> putStrLn (render (ppProgram ast))
    else if optCheck opts
      then do
        -- Executa o parser e o type checker
        case slParser content of
          Left err -> putStrLn err >> exitFailure
          Right ast -> case checkProgram ast of
            Left err -> putStrLn err >> exitFailure
            Right _ -> putStrLn "Type check passed"
    else
      -- Mensagem caso nenhuma flag seja fornecida
      putStrLn "Please specify one of --lexer, --parser, --pretty, --check"

-- Configuracao da descricao do programa para o help
optsDef :: ParserInfo Options
optsDef = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc "SL Compiler"
  <> header "bcc328 - SL Compiler" )

-- Helper para converter a AST (Program) para Data.Tree para visualizacao com drawTree
astToTree :: Program -> Tree String
astToTree (Program topLevels) = Node "Program" (map topLevelToTree topLevels)

-- Converte declaracoes de topo (Funcoes ou Structs)
topLevelToTree :: TopLevel -> Tree String
topLevelToTree (TopFunc func) = funcToTree func
topLevelToTree (TopStruct struct) = structToTree struct

-- Converte declaracao de Struct para arvore
structToTree :: StructDecl -> Tree String
structToTree (StructDecl name fields) = Node ("Struct: " ++ name) (map fieldToTree fields)

-- Converte campos da Struct
fieldToTree :: (String, Type) -> Tree String
fieldToTree (name, typ) = Node ("Field: " ++ name) [typeToTree typ]

-- Converte declaracao de Funcao para arvore
funcToTree :: FuncDecl -> Tree String
funcToTree (FuncDecl name generics params ret body) = 
  Node ("Func: " ++ name) 
    [ Node ("Generics: " ++ show generics) []
    , Node "Params" (map paramToTree params)
    , Node "ReturnType" [typeToTree ret]
    , Node "Body" (map stmtToTree body)
    ]

-- Converte parametros da funcao
paramToTree :: (String, Type) -> Tree String
paramToTree (name, typ) = Node ("Param: " ++ name) [typeToTree typ]

-- Converte comandos (Statements) para arvore
stmtToTree :: Stmt -> Tree String
stmtToTree (SLet name typ expr) = Node ("Let: " ++ name) (typeToTree typ : maybe [] ((:[]) . exprToTree) expr)
stmtToTree (SAssign lhs rhs) = Node "Assign" [exprToTree lhs, exprToTree rhs]
stmtToTree (SReturn expr) = Node "Return" (maybe [] ((:[]) . exprToTree) expr)
stmtToTree (SIf cond thenBlk elseBlk) = Node "If" (exprToTree cond : Node "Then" (map stmtToTree thenBlk) : maybe [] (\b -> [Node "Else" (map stmtToTree b)]) elseBlk)
stmtToTree (SWhile cond body) = Node "While" [exprToTree cond, Node "Body" (map stmtToTree body)]
-- Note: A condicao do For aqui assume que nao eh Maybe (diferente de outras versoes), seguindo o codigo fornecido
stmtToTree (SFor forInit cond step body) = Node "For" [Node "Init" (maybe [] ((:[]) . stmtToTree) forInit), Node "Cond" [exprToTree cond], Node "Step" (maybe [] ((:[]) . stmtToTree) step), Node "Body" (map stmtToTree body)]
stmtToTree (SPrint expr) = Node "Print" [exprToTree expr]
stmtToTree (SExp expr) = Node "StmtExp" [exprToTree expr]

-- Converte expressoes para arvore (usa show para tipos primitivos)
exprToTree :: Expr -> Tree String
exprToTree (EInt i) = Node ("Int: " ++ show i) []
exprToTree (EFloat f) = Node ("Float: " ++ show f) []
exprToTree (EString s) = Node ("String: " ++ show s) []
exprToTree (EBool b) = Node ("Bool: " ++ show b) []
exprToTree (EVar s) = Node ("Var: " ++ s) []
exprToTree (EBinOp op l r) = Node ("BinOp: " ++ show op) [exprToTree l, exprToTree r]
exprToTree (EUnOp op e) = Node ("UnOp: " ++ show op) [exprToTree e]
exprToTree (ECall f args) = Node "Call" (exprToTree f : map exprToTree args)
exprToTree (EArrayAccess arr idx) = Node "ArrayAccess" [exprToTree arr, exprToTree idx]
exprToTree (EFieldAccess obj f) = Node ("FieldAccess: " ++ f) [exprToTree obj]
exprToTree (ENew t) = Node "New" [typeToTree t]
exprToTree (EStructInit n args) = Node ("StructInit: " ++ n) (map exprToTree args)
exprToTree (EArrayLit exprs) = Node "ArrayLit" (map exprToTree exprs)

-- Converte tipos para arvore
typeToTree :: Type -> Tree String
typeToTree (TyArray t (Just e)) = Node "Array" [typeToTree t, Node ("Size") [exprToTree e]]
typeToTree (TyArray t Nothing) = Node "Array" [typeToTree t]
typeToTree t = Node (show t) []