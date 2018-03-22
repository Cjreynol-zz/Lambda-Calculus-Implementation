{-|
Module      : Pure.Parser
Description : Parser for locally nameless untyped lambda terms
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
Description : No spaces, \\ . to represent lambda binders.  Bound variables 
                are represented as natural numbers referencing their binder.  
                Free variables  are also represented by natural numbers, but 
                with an 'f' character preceding them.  Application is 
                implicit with adjacent terms, and parenthesis can be used to 
                end the binding scope of a lambda.  
-}
module Pure.Parser(
    termParserChecker
    ) where


import Data.Bifunctor           (first)
import Data.Char                (digitToInt)
import Text.Parsec              (ParseError, char, digit, many1, parse, (<|>))
import Text.Parsec.String       (Parser)

import Nat                      (intToNat) 
import Pure.Term                (Term(..), locallyClosedCheck)
import SimpleTypes.TypingError  (ErrorString)


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

termParser :: String -> Either ParseError Term
termParser input = parse parseTerms "" input

-- | Composes parsing and checking that terms are well formed, returns the 
-- result or an error to display.
termParserChecker :: String -> Either ErrorString Term
termParserChecker input = first show (termParser input) >>= locallyClosedCheck
