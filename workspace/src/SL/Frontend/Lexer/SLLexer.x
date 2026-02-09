{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module SL.Frontend.Lexer.SLLexer where

import Control.Monad
import SL.Frontend.Lexer.Token
}

--nao usado o monadUserState pois a linguagem SL nao usa de comentarios aninhads
%wrapper "monad"

--definicao de macros de ER
--primitivas
$digit = 0-9
$alpha = [a-zA-Z]
--compostas
@identifier = $alpha[$alpha $digit]*
@number = $digit+
@float = $digit+ \. $digit+
@string = \" .* \"

--padroes de expressoes regulares
tokens :-
    --espacos em branco e comentarios de linha
    <0> $white+     ;
    <0> "//" .*     ;

    --palavras chave
    <0> "func"    {simpleToken TFunc}
    <0> "struct"  {simpleToken TStruct}
    <0> "if"      {simpleToken TIf}
    <0> "else"    {simpleToken TElse}
    <0> "while"   {simpleToken TWhile}
    <0> "for"     {simpleToken TFor}
    <0> "return"  {simpleToken TReturn}
    <0> "let"     {simpleToken TLet}
    <0> "new"     {simpleToken TNew}
    <0> "print"   {simpleToken TPrint}
    <0> "forall"  {simpleToken TForall}
    <0> "true"    {simpleToken TTrue}
    <0> "false"   {simpleToken TFalse}
    <0> "void"    {simpleToken TVoid}
    <0> "int"     {simpleToken TInt}
    <0> "float"   {simpleToken TFloat}
    <0> "string"  {simpleToken TString}
    <0> "bool"    {simpleToken TBool}
    
    --definicao de tipos
    <0> "->"      {simpleToken TArrow}

    --operadores
    <0> "++"      {simpleToken TInc}
    <0> "+"       {simpleToken TPlus}
    <0> "-"       {simpleToken TMinus}
    <0> "*"       {simpleToken TTimes}
    <0> "/"       {simpleToken TDiv}
    <0> "&&"      {simpleToken TAnd}
    <0> "||"      {simpleToken TOr}
    <0> "!="      {simpleToken TNe}
    <0> "!"       {simpleToken TNot}
    <0> ">="      {simpleToken TGe}
    <0> "<="      {simpleToken TLe}
    <0> ">"       {simpleToken TGt}
    <0> "<"       {simpleToken TLt}
    <0> "=="      {simpleToken TEq}
    
    --simbolos
    <0> "{"       {simpleToken TLBrace}
    <0> "}"       {simpleToken TRBrace}
    <0> "("       {simpleToken TLParen}
    <0> ")"       {simpleToken TRParen}
    <0> "["       {simpleToken TLBracket}
    <0> "]"       {simpleToken TRBracket}
    <0> ":"       {simpleToken TColon}
    <0> ","       {simpleToken TComma}
    <0> ";"       {simpleToken TSemi}
    <0> "="       {simpleToken TAssign}
    
    --valores literais
    <0> @float    {mkFloat}
    <0> @number   {mkInt}
    <0> @string   {mkString}
    
    <0> "."       {simpleToken TDot}

    --identificadores literais
    <0> @identifier {mkIdent}

    --toef nao e um padrao de expressao regular
--funcao chamada ao atingir fim de entrada
{
alexEOF :: Alex Token
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  pure $ Token (position pos) TEOF

--converte a posicao do alex para (linha, coluna)
position :: AlexPosn -> (Int, Int)
position (AlexPn _ x y) = (x, y)

--anexa a posicao ao lexema
simpleToken :: Lexeme -> AlexAction Token
simpleToken lx (st, _, _, _) _
  = return $ Token (position st) lx

--cria um token para o literal float
mkFloat :: AlexAction Token
mkFloat (st, _, _, str) len
  = pure $ Token (position st) (TLitFloat $ read $ take len str)

--cria um token para o literal int
mkInt :: AlexAction Token
mkInt (st, _, _, str) len
  = pure $ Token (position st) (TLitInt $ read $ take len str)

--cria um token para o literal de string
mkString :: AlexAction Token
mkString (st, _, _, str) len
  = pure $ Token (position st) (TLitString $ take (len -2) (drop 1 str))

--cria um token para o identificador
mkIdent :: AlexAction Token
mkIdent (st, _, _, str) len
  = pure $ Token (position st) (TIdent $ take len str)

--funcao principal do lexer
--pega uma string de codigo de entrada e a transforma em uma lista sequencial de tokens
lexer :: String -> Either String [Token]
lexer s = runAlex s go
  where
    go = do
      output <- alexMonadScan
      if lexeme output == TEOF then
        pure [output]
      else (output :) <$> go
}