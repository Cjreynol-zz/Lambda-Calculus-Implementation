{-|
Module      : Parsing.ParseableTerm
Description : Default parsing functions for most of LambdaTerm constructors
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module Parsing.ParseableTerm(
    ParseableTerm(..)
    ) where


import Data.Char                    (digitToInt)
import Text.Parsec                  (char, digit, many1, (<|>))
import Text.Parsec.String           (Parser)

import LambdaTerm                   (LambdaTerm(..))
import Nat                          (intToNat)


-- | Provides default implementations for constructing a parser of 
-- LambdaTerms of different types.
class ParseableTerm ty where
    parseLam :: Parser (LambdaTerm ty)

    parseBVar :: Parser (LambdaTerm ty)
    parseBVar = do
                nat <- digit
                return $ BVar (intToNat (digitToInt nat))

    parseFVar :: Parser (LambdaTerm ty)
    parseFVar = do
                    _ <- char 'f'
                    nat <- digit
                    return $ FVar (intToNat (digitToInt nat))

    parseParenTerm :: Parser (LambdaTerm ty)
    parseParenTerm = do
                        _ <- char '('
                        term <- parseTerms
                        _ <- char ')'
                        return term

    parseTerm :: Parser (LambdaTerm ty)
    parseTerm = parseParenTerm <|> parseLam <|> parseFVar <|> parseBVar 

    parseTerms :: Parser (LambdaTerm ty)
    parseTerms = do
                    applications <- many1 parseTerm
                    return $ foldl1 App applications

