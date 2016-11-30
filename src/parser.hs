module LambdaParser where

import Data.Char

import Text.Parsec
import Text.Parsec.String

import Term
import Nat


parseTerm :: Parser Term
parseTerm = do  spaces
                char '\\'
                spaces
                char '.'
                spaces
                term <- parseTerm
                spaces
                return (Lam term)
        <|> do  appls <- many1 parseVarParens
                return (foldl1 App appls)

parseVarParens :: Parser Term
parseVarParens = do spaces
                    variable <- digit
                    spaces
                    return (Var (int2nat (digitToInt variable)))
            <|> do  char '('
                    spaces
                    term <- parseTerm
                    spaces
                    char ')'
                    return term
