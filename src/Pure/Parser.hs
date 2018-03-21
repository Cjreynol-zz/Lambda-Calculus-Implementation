{-|
Module      : Pure.Parser
Description : Parser for locally nameless untyped lambda terms
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module Pure.Parser(
    termParser
    ) where


import Data.Bifunctor       (second)
import Data.Char            (digitToInt)
import Text.Parsec          (ParseError, char, digit, many1, parse, (<|>))
import Text.Parsec.String   (Parser)

import Pure.Term            (Term(..), locallyClosedCheck)
import Nat                  (intToNat) 


parseLam :: Parser Term
parseLam = do
            _ <- char '\\'
            _ <- char '.'
            term <- parseTerms
            return $ Lam term

parseBVar :: Parser Term
parseBVar = do
            nat <- digit
            return $ BVar (intToNat (digitToInt nat))

parseFVar :: Parser Term
parseFVar = do
                _ <- char 'f'
                nat <- digit
                return $ FVar (intToNat (digitToInt nat))

parseParenTerm :: Parser Term
parseParenTerm = do
                    _ <- char '('
                    term <- parseTerms
                    _ <- char ')'
                    return $ term

parseTerm :: Parser Term
parseTerm = parseParenTerm <|> parseLam <|> parseFVar <|> parseBVar 

parseTerms :: Parser Term
parseTerms = do
                applications <- many1 parseTerm
                return $ foldl1 App applications

-- | Parser for Terms.  
--
-- No spaces, \\ . to represent lambda binders.  Bound variables are 
-- represented as natural numbers referencing their binder.  Free variables 
-- are also represented by natural numbers, but with an 'f' character 
-- preceding them.  Application is implicit with adjacent terms, and 
-- parenthesis can be used to end the binding scope of a lambda.  
-- 
-- For example:  
-- 
-- \\ .00 is equivalent to (\\ .(0 0)) while (\\ .0)f0 is equivalent to 
-- (\\ .0) 0), which is a beta redex where the 0 outside the lambda 
-- abstraction is a free variable.
termParser :: String -> Either ParseError (Either String Term)
termParser input = second locallyClosedCheck $ parse parseTerms "" input

