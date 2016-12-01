module Parser where

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
        <|> do  spaces
                appls <- many1 parseVarParens
                spaces
                return (foldl1 App appls)

parseVarParens :: Parser Term
parseVarParens = do spaces
                    variable <- digit
                    spaces
                    return (Var (intToNat (digitToInt variable)))
            <|> do  spaces
                    char '('
                    spaces
                    term <- parseTerm
                    spaces
                    char ')'
                    spaces
                    return term

myParser :: String -> Either ParseError Term
myParser input = parse parseTerm "" input
