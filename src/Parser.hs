{-|
Module      : Parser
Description : Parsing functions for Terms
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module Parser(
    termParser
    ) where


import Data.Char            (digitToInt)
import Text.Parsec          (ParseError, char, digit, many1, parse, (<|>))
import Text.Parsec.String   (Parser)

import Term                 (Term(..))
import Nat                  (intToNat) 


parseLam :: Parser Term
parseLam = do
            _ <- char '\\'
            _ <- char '.'
            term <- parseTerms
            return $ Lam term

parseVar :: Parser Term
parseVar = do
            var <- digit
            return $ Var (intToNat (digitToInt var))

parseParenTerm :: Parser Term
parseParenTerm = do
                    _ <- char '('
                    term <- parseTerms
                    _ <- char ')'
                    return $ term

parseTerm :: Parser Term
parseTerm = parseParenTerm <|> parseLam <|> parseVar

parseTerms :: Parser Term
parseTerms = do
                applications <- many1 parseTerm
                return $ foldl1 App applications

-- | Parser for Terms.  
--
-- No spaces, \\ . to represent lambda binders.  Vars are represented as 
-- natural numbers referencing their binder.  That binder could be implicit 
-- with free variables in the terms.  Application is implicit with adjacent 
-- terms, and parenthesis can be used to end the binding scope of a lambda.  
-- 
-- For example:  
-- 
-- \\ .00 is equivalent to (\\ .(0 0)) while (\\ .0)0 is equivalent to (\\ .0) 0), 
-- which is a beta redex.
termParser :: String -> Either ParseError Term
termParser input = parse parseTerms "" input
