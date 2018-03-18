{-|
Module      : Parser
Description : Parsing functions for Terms
Copyright   : (c) Chad Reynolds, 2018
-}
module Parser(
    termParser
    ) where


import Data.Char            (digitToInt)
import Text.Parsec          (ParseError, char, digit, many1, parse, spaces, 
                                (<|>))
import Text.Parsec.String   (Parser)

import Term                 (Term(..))
import Nat                  (intToNat) 


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

-- | Parser for Terms, currently parses improperly on some terms so full 
-- parenthesizing is needed for guaranteed accuracy.
termParser :: String -> Either ParseError Term
termParser input = parse parseTerm "" input
