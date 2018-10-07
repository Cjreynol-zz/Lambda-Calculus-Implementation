{-|
Module      : Parsing.ParseableContext
Description : Default parsing functions for typing contexts
Copyright   : (c) Chad Reynolds, 2018
License     : MIT

A typing context is expected first, wrapped in square brackets [].  The 
elements inside are pairs (n,type) where n is a natural number and type is a 
valid type.  Consecutive pairs do not have any delimiter, and in the case of 
multiple pairs containing the same number n, the type associated with that 
variable in the context will be the last in the list.

Types are expected to be characters, typically uppercase, separated by 
arrows, ->.

For terms, no spaces, \\:type. to represent lambda binders.  Bound variables 
are represented as natural numbers referencing their binder as is standard in 
De Bruijn notation.  Free variables are also represented by natural numbers, 
but with an 'f' preceding them.  Application is implicit with adjacent terms, 
and parenthesis can be used to end the binding scope of a lambda.  
-}
module Parsing.ParseableContext(
      ParseableContext(..)
--    , termParserChecker
    ) where

import Data.Char                (digitToInt)
import Text.Parsec              ((<|>), char, digit, letter, many, string)
import Text.Parsec.String       (Parser)

import Context                  (Context, newContext)
import Nat                      (Atom)
import qualified    SimpleTypes as ST   (Type(..))


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
                            return ((toEnum (digitToInt nat)),typ)

instance ParseableContext ST.Type where
    parseTypes = do
        typ <- parseType
        types <- many parseArrow
        case types of
            [] -> return typ
            _ -> return $ foldr1 ST.Arrow (typ:types)


parseTypeVar :: Parser ST.Type
parseTypeVar = do
                var <- letter
                return $ ST.TypeVar var

parseType :: Parser ST.Type
parseType = parseParenType <|> parseTypeVar

parseArrow :: Parser ST.Type
parseArrow = do
                _ <- string "->"
                ty2 <- parseType
                return $ ty2

parseParenType :: Parser ST.Type
parseParenType = do
                    _ <- char '('
                    typ <- parseTypes
                    _ <- char ')'
                    return typ

