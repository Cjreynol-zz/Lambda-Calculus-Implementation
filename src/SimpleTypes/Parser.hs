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
import Text.Parsec          (ParseError, char, digit, many1, parse, (<|>))
import Text.Parsec.String   (Parser)

import Pure.Term            (Atom)
import SimpleTypes.Term     (Term(..), Type(..), Context, typeCheck, 
                                newContext)
import Nat                  (intToNat) 


parseContext :: Parser Context
parseContext = undefined

parseContextEntry :: Parser (Atom, Type)
parseContextEntry = undefined

parseArrow :: Parser Type
parseArrow = undefined

parseTypeVar :: Parser Type
parseTypeVar = undefined

parseType :: Parser Type
parseType = undefined

parseParenType :: Parser Type
parseParenType = undefined

parseTypes :: Parser Type
parseTypes = undefined

parseLam :: Parser Term
parseLam = undefined --do
--            _ <- char '\\'
--            _ <- char '.'
--            term <- parseTerms
--            return $ Lam term

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
termParser :: String -> Either ParseError (Either String Term)
termParser input = undefined--second locallyClosedCheck $ parse parseTerms "" input
