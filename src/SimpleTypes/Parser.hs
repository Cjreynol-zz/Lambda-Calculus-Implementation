{-|
Module      : SimpleTypes.Parser
Description : Parsing functions for simply typed lambda calculus terms
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module SimpleTypes.Parser(
    termParser
    ) where


import Data.Bifunctor       (second)
import Data.Char            (digitToInt)
import Text.Parsec          (ParseError, char, digit, letter, many, many1, 
                                parse, (<|>))
import Text.Parsec.String   (Parser)

import Nat                  (Atom, intToNat)
import SimpleTypes.Context  (Context, newContext)
import SimpleTypes.Term     (Term(..), typeCheck)
import SimpleTypes.Type     (Type(..))


parseContext :: Parser Context
parseContext = do
                _ <- char '['
                pairs <- many parseContextEntry
                _ <- char ']'
                _ <- char '|'
                _ <- char '-'
                return $ newContext pairs

parseContextEntry :: Parser (Atom, Type)
parseContextEntry = do
                        _ <- char '('
                        nat <- digit
                        _ <- char ','
                        typ <- parseTypes
                        _ <- char ')'
                        return ((intToNat (digitToInt nat)),typ)

parseTypeVar :: Parser Type
parseTypeVar = do
                var <- letter
                return $ TypeVar var

parseType :: Parser Type
parseType = parseParenType <|> parseTypeVar

parseParenType :: Parser Type
parseParenType = do
                    _ <- char '('
                    typ <- parseType
                    _ <- char ')'
                    return typ

parseTypes :: Parser Type
parseTypes = do
                types <- many1 parseType
                return $ foldl1 Arrow types

parseLam :: Parser Term
parseLam = do
            _ <- char '\\'
            _ <- char ':'
            typ <- parseTypes
            _ <- char '.'
            term <- parseTerms
            return $ Lam typ term

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
                    return term

parseTerm :: Parser Term
parseTerm = parseParenTerm <|> parseLam <|> parseFVar <|> parseBVar 

parseTerms :: Parser Term
parseTerms = do
                applications <- many1 parseTerm
                return $ foldl1 App applications

parseAll :: Parser (Term,Context)
parseAll = do
            context <- parseContext
            term <- parseTerms
            return (term,context)

-- | Parser for Terms.  
--
-- Need to adjust types so that I can return a type/term pair for display.
termParser :: String -> Either ParseError (Either String Type)
termParser input = second helper $ parse parseAll "" input
    where
        helper (t,c) = typeCheck t c
