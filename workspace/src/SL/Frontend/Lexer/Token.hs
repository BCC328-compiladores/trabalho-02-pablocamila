module SL.Frontend.Lexer.Token where

-- definição do token

data Token 
    = Token {
        pos :: (Int, Int), --linha e coluna no código
        lexeme :: Lexeme
    } deriving (Eq, Ord, Show)

data Lexeme
    --tipos
    = TInt
    | TFloat
    | TBool
    | TString
    --estruturas de controle
    | TIf
    | TElse
    | TForall
    | TFor
    | TWhile
    --estruras de dados
    | TStruct
    | TNew --array
    --funções
    | TFunc
    | TReturn
    | TVoid
    --instanciar variaveis
    | TLet
    --operadores
    | TPlus
    | TMinus
    | TTimes
    | TDiv
    | TInc -- (++)
    | TAnd
    | TOr
    | TNot
    | TGt --(> grater than)
    | TLt --(<)
    | TGe --(>=)
    | TLe --(<=)
    | TEq --(==)
    | TNe --(!=)
    --símbolos
    | TLBrace --({ chaves)
    | TRBrace
    | TLParen
    | TRParen
    | TRBracket
    | TLBracket --([ colchetes)
    | TColon --(:)
    | TComma --(,)
    | TDot --(.)
    | TSemi --(; semi colon)
    | TAssign --(=)
    | TArrow --(-> Declaração de tipo)
    --printar na tela
    | TPrint
    --valores literais
    | TTrue
    | TFalse
    | TLitInt Int
    | TLitFloat Float
    | TLitString String
    --identificadores literais
    | TIdent String
    | TEOF
    deriving (Eq, Ord, Show)