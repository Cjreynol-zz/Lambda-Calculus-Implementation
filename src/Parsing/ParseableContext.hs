{-|
Module      : Parsing.ParseableContext
Description : Default parsing functions for typing contexts
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module Parsing.ParseableContext(
    ParseableContext(..)
    ) where


import Data.Char                (digitToInt)
import Text.Parsec              (char, digit, many, string)
import Text.Parsec.String       (Parser)

import Context                  (Context, newContext)
import Nat                      (Atom, intToNat)


-- | Provides parsing for typing contexts when provided with function to 
-- parse types
class ParseableContext ty where

    parseTypes :: Parser ty

    parseContext :: Parser (Context ty)
    parseContext = do
                    _ <- char '['
                    pairs <- many parseContextEntry
                    _ <- string "]|-"
                    return $ newContext pairs

    parseContextEntry :: Parser (Atom, ty)
    parseContextEntry = do
                            _ <- char '('
                            nat <- digit
                            _ <- char ','
                            typ <- parseTypes
                            _ <- char ')'
                            return ((intToNat (digitToInt nat)),typ)
