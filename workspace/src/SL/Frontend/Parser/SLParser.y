-- Imports e diretivas principais pro happy
{
module SL.Frontend.Parser.SLParser (slParser) where

import SL.Frontend.Lexer.Token
import SL.Frontend.Lexer.SLLexer hiding (lexer)
import SL.Frontend.Syntax.SLSyntax
}


%name parser Program
%monad { Alex } { (>>=) } { return }
%tokentype { Token }
%error { parseError }
%lexer { lexer } { Token _ TEOF }

-- Lista de Tokens chamando construtur do lexer
-- Os _ ignora a posicao acumulada nos tokens, so importa o tipo

%token
  'func'    { Token _ TFunc }
  'struct'  { Token _ TStruct }
  'if'      { Token _ TIf }
  'else'    { Token _ TElse }
  'while'   { Token _ TWhile }
  'for'     { Token _ TFor }
  'return'  { Token _ TReturn }
  'let'     { Token _ TLet }
  'new'     { Token _ TNew }
  'print'   { Token _ TPrint }
  'forall'  { Token _ TForall }
  'true'    { Token _ TTrue }
  'false'   { Token _ TFalse }
  'void'    { Token _ TVoid }
  'int'     { Token _ TInt }
  'float'   { Token _ TFloat }
  'string'  { Token _ TString }
  'bool'    { Token _ TBool }
  '{'       { Token _ TLBrace }
  '}'       { Token _ TRBrace }
  '('       { Token _ TLParen }
  ')'       { Token _ TRParen }
  '['       { Token _ TLBracket }
  ']'       { Token _ TRBracket }
  ';'       { Token _ TSemi }
  ':'       { Token _ TColon }
  '.'       { Token _ TDot }
  ','       { Token _ TComma }
  '='       { Token _ TAssign }
  '->'      { Token _ TArrow }
  '++'      { Token _ TInc }
  '+'       { Token _ TPlus }
  '-'       { Token _ TMinus }
  '*'       { Token _ TTimes }
  '/'       { Token _ TDiv }
  '&&'      { Token _ TAnd }
  '||'      { Token _ TOr }
  '!'       { Token _ TNot }
  '>'       { Token _ TGt }
  '<'       { Token _ TLt }
  '>='      { Token _ TGe }
  '<='      { Token _ TLe }
  '=='      { Token _ TEq }
  '!='      { Token _ TNe }

-- $$ usado para desencapsular os valores dentro destes tipos
  ident     { Token _ (TIdent $$) }
  int       { Token _ (TLitInt $$) }
  float     { Token _ (TLitFloat $$) }
  str       { Token _ (TLitString $$) }

-- Ordens de precedencia
-- Usadas para reduzir os erros de shift/reduce
-- cada % indica o motivo de ter adicionado para nao apresentar os erros indicados pelo Happy
%nonassoc LOW_PREC
%right '->'                             -- (operador  ->, ex: int -> (int -> int) ) Associativo a direita
%left '||'                              -- Precedencia a esquerda. ex: (a || b || c vira (a || b) || c )
%left '&&'                              -- && tem precedencia maior que Ou, por isso abaixo
%nonassoc '==' '!=' '>' '<' '>=' '<='   -- Sao nao associativos, necessitando de parenteses

-- Precedencias de aritmetica normal 
%left '+' '-'                           
%left '*' '/'

-- Precedencia de operadores unarios de negacao 
%left '!' NEG -- neg permite definir precedencia de negacao aritmetica entre operador binario e unario

-- Precedencia de acessar campo de objeto e os simbolos da ordens de realizar conta matematica
%left '.' '[' '('
%%

--------- Codigo de Gramatica construido baseada na gramatica desenvolvida antews ---------
-- Cada $(posicao_na_declaracao) diz o node que estara presente na arvore

-- Um Program e composto por uma lista de TopLevels
-- TopLevels e uma lista recursiva: um TopLevel seguido de mais TopLevels
-- Gramatica: prog -> tops prog | lambda
Program
  : TopLevels { Program $1 }

TopLevels
  : TopLevel TopLevels { $1 : $2 }
  | {- empty -} { [] }

-- Gramatica: top -> funcDecl | structDecl
TopLevel
  : FuncDecl { TopFunc $1 }
  | StructDecl { TopStruct $1 }

-- Structs
-- Gramatica: structDecl -> 'struct' ID '{' structFields '}'
StructDecl
  : 'struct' ident '{' StructFields '}' { StructDecl $2 $4 }

-- Gramatica: structFields -> structField structFields | lambda
StructFields
  : StructField StructFields { $1 : $2 }
  | {- empty -} { [] }

-- Gramatica: structField -> ID ':' type ';'
StructField
  : ident ':' Type ';' { ($1, $3) }

-- Funcoes pode ser com ou sem genericos
-- Gramatica: funcDecl -> generic ... | 'func' ...
FuncDecl
  : Generics 'func' ident '(' Params ')' ReturnType '{' Block '}'
    { FuncDecl $3 $1 $5 $7 $9 }
  | 'func' ident '(' Params ')' ReturnType '{' Block '}'
    { FuncDecl $2 [] $4 $6 $8 }

-- Genericos pode conter o tipo explicito ou nao 
-- Gramatica: generic -> 'forall' typeVar '.'
Generics
  : 'forall' TypeVars '.' { $2 }

-- Gramatica: typeVar -> ID typeVar | ID
TypeVars
  : ident TypeVars { $1 : $2 }
  | ident { [$1] }

-- Gramatica: params -> paramList | lambda
Params
  : ParamList { $1 }
  | {- empty -} { [] }

-- Parametros
-- Gramatica: paramList -> param ',' paramList | param
ParamList
  : Param ',' ParamList { $1 : $3 }
  | Param { [$1] }

-- Gramatica: param -> ID ':' type | ID
Param
  : ident ':' Type { ($1, $3) }
  | ident { ($1, TyVar "auto") } -- Inferencia de tipo

-- Tipo Retornado 
-- Gramatica: returnType -> ':' type | lambda
ReturnType
  : ':' Type { $2 }
  | {- empty -} { TyVoid } -- void se nao conter


-- Tipos
-- Todos tipos que uma variavel pode assumir
-- Gramatica: int | float | ... | type '[' ']' | type '[' expr ']' | ...
Type
  : 'int' { TyInt }
  | 'float' { TyFloat }
  | 'string' { TyString }
  | 'bool' { TyBool }
  | 'void' { TyVoid }
  | Type '[' ']' { TyArray $1 Nothing }         -- Array sem tamanho (int[])
  | ident { if $1 `elem` ["int","float","string","bool","void"] then error "Parser logic error" else TyStruct $1 } -- Structs ou erro se for palavra reservada
  | Type '[' Expr ']' { TyArray $1 (Just $3) }  -- Array com tamanho (int[10])
  | '(' Types ')' '->' Type { TyFun $2 $5 }     -- Tipo funcao

Types
  : Type ',' Types { $1 : $3 }
  | Type { [$1] }

-- Blocos e comandos contem o conteudo presentes nas funcoes
-- Um bloco e uma lista de comandos.
-- Gramatica: block -> cmds
Block
  : Stmts { $1 }

Stmts
  : Stmt Stmts { $1 : $2 }
  | {- empty -} { [] }

-- Gramatica: cmd -> let ... | if ... | while ... | print ...
Stmt
  : 'let' ident ':' Type '=' Expr ';' { SLet $2 $4 (Just $6) }      -- Declaracao com tipo e valor
  | 'let' ident ':' Type ';' { SLet $2 $4 Nothing }                 -- Declaracao sem valor
  | 'let' ident '=' Expr ';' { SLet $2 (TyVar "auto") (Just $4) }   -- Declaracao com inferencia
  | Expr '=' Expr ';' { SAssign $1 $3 }                             -- Atribuicao
  | 'return' Expr ';' { SReturn (Just $2) }
  | 'return' ';' { SReturn Nothing }
  | 'if' '(' Expr ')' '{' Block '}' 'else' '{' Block '}' { SIf $3 $6 (Just $10) }
  | 'if' '(' Expr ')' '{' Block '}' { SIf $3 $6 Nothing }
  | 'while' '(' Expr ')' '{' Block '}' { SWhile $3 $6 }
  | 'for' '(' ForInit ';' Expr ';' ForStep ')' '{' Block '}' { SFor $3 $5 $7 $10 }
  | 'print' '(' Expr ')' ';' { SPrint $3 }
  | Expr ';' { SExp $1 }

-- Tipos de condicoes no laco do For (inicial e step podem ser vazias)
ForInit
  : 'let' ident ':' Type '=' Expr  { Just (SLet $2 $4 (Just $6)) }
  | 'let' ident ':' Type           { Just (SLet $2 $4 Nothing) }
  | 'let' ident '=' Expr           { Just (SLet $2 (TyVar "auto") (Just $4)) }
  | Expr '=' Expr                  { Just (SAssign $1 $3) }
  | Expr                           { Just (SExp $1) }
  | {- empty -}                    { Nothing }

ForStep
  : ident '=' Expr { Just (SAssign (EVar $1) $3) }
  | ident '++' { Just (SAssign (EVar $1) (EBinOp Add (EVar $1) (EInt 1))) }
  | {- empty -} { Nothing }

-- -- usava opt mas agora o segundo parametro e obrigatorio no for
-- OptExpr
--   : Expr { Just $1 }
--   | {- empty -} { Nothing }

-- Expressoes
-- Gramatica: lista longa de operacoes binarias e unarias.
-- Aqui contem as precedencias escritas anteriormentes
Expr
  : int { EInt $1 }
  | float { EFloat $1 }
  | str { EString $1 }
  | 'true' { EBool True }
  | 'false' { EBool False }
  | ident { EVar $1 }
  | Expr '+' Expr { EBinOp Add $1 $3 }
  | Expr '-' Expr { EBinOp Sub $1 $3 }
  | Expr '*' Expr { EBinOp Mul $1 $3 }
  | Expr '/' Expr { EBinOp Div $1 $3 }
  | Expr '&&' Expr { EBinOp And $1 $3 }
  | Expr '||' Expr { EBinOp Or $1 $3 }
  | Expr '>' Expr { EBinOp Gt $1 $3 }
  | Expr '<' Expr { EBinOp Lt $1 $3 }
  | Expr '>=' Expr { EBinOp Ge $1 $3 }
  | Expr '<=' Expr { EBinOp Le $1 $3 }
  | Expr '==' Expr { EBinOp Eq $1 $3 }
  | Expr '!=' Expr { EBinOp Ne $1 $3 }
  | '!' Expr { EUnOp Not $2 }
  | '-' Expr %prec NEG { EUnOp Neg $2 }
  | Expr '(' Exprs ')' { ECall $1 $3 }
  | Expr '(' ')' { ECall $1 [] }
  | Expr '[' Expr ']' { EArrayAccess $1 $3 }
  | Expr '.' ident { EFieldAccess $1 $3 }
  | 'new' Type %prec LOW_PREC { ENew $2 }
  | ident '{' Exprs '}' { EStructInit $1 $3 }
  | '[' Exprs ']' { EArrayLit $2 }
  | '(' Expr ')' { $2 }

Exprs
  : Expr ',' Exprs { $1 : $3 }
  | Expr { [$1] }

-- Funcoes auxiliares para encontrar erros, monada do alex e leitura de string para montar a AST
{
parseError :: Token -> Alex a
parseError (Token (l, c) t) = alexError $ "Parse error at line " ++ show l ++ ", column " ++ show c ++ ": " ++ show t

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)

slParser :: String -> Either String Program
slParser s = runAlex s parser
}