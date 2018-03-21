module LNParser(
    lnTermParser
    ) where


import Data.Bifunctor       (second)
import Data.Char            (digitToInt)
import Text.Parsec          (ParseError, char, digit, many1, parse, (<|>))
import Text.Parsec.String   (Parser)

import LNTerm               (LNTerm(..), locallyClosedCheck)
import Nat                  (intToNat) 


parseLam :: Parser LNTerm
parseLam = do
            _ <- char '\\'
            _ <- char '.'
            term <- parseLNTerms
            return $ Lam term

parseBVar :: Parser LNTerm
parseBVar = do
            nat <- digit
            return $ BVar (intToNat (digitToInt nat))

parseFVar :: Parser LNTerm
parseFVar = do
                _ <- char 'f'
                nat <- digit
                return $ FVar (intToNat (digitToInt nat))

parseParenLNTerm :: Parser LNTerm
parseParenLNTerm = do
                    _ <- char '('
                    term <- parseLNTerms
                    _ <- char ')'
                    return $ term

parseLNTerm :: Parser LNTerm
parseLNTerm = parseParenLNTerm <|> parseLam <|> parseFVar <|> parseBVar 

parseLNTerms :: Parser LNTerm
parseLNTerms = do
                applications <- many1 parseLNTerm
                return $ foldl1 App applications

-- | Parser for LNTerms.  
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
lnTermParser :: String -> Either ParseError (Either String LNTerm)
lnTermParser input = second locallyClosedCheck $ parse parseLNTerms "" input
