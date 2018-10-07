{-|
Module      : Parsing.ParseableTerm
Description : Default parsing functions for most of LambdaTerm constructors
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module Parsing.ParseableTerm(
      ParseableTerm(..)
    , pureTermParserChecker
    , termParserChecker
    ) where


import              Data.Bifunctor              (first)
import              Data.Char                   (digitToInt)
import              Text.Parsec                 (ParseError, (<|>), char, 
                                                    digit, many1, parse, 
                                                    string)
import              Text.Parsec.String          (Parser)

import              Context                     (Context)
import              LambdaTerm                  (LambdaTerm(..), PureTerm, 
                                                    locallyClosedCheck)
import              Parsing.ParseableContext    (ParseableContext, 
                                                    parseContext, parseTypes)
import qualified    SimpleTypes as ST           (Type, Term, typeCheckAndTerm)


-- | Provides default implementations for constructing a parser of 
-- LambdaTerms of different types.
class ParseableTerm ty where
    parseLam :: Parser (LambdaTerm ty)

    parseBVar :: Parser (LambdaTerm ty)
    parseBVar = do
                nat <- digit
                return $ BVar (toEnum (digitToInt nat))

    parseFVar :: Parser (LambdaTerm ty)
    parseFVar = do
                    _ <- char 'f'
                    nat <- digit
                    return $ FVar (toEnum (digitToInt nat))

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

instance ParseableTerm () where
    parseLam = do
                _ <- char '\\'
                _ <- char '.'
                term <- parseTerms
                return $ Lam () term

instance ParseableTerm ST.Type where
    parseLam = do
                _ <- string "\\:"
                typ <- parseTypes 
                _ <- char '.'
                term <- parseTerms
                return $ Lam typ term

pureTermParser :: String -> Either ParseError PureTerm
pureTermParser input = parse parseTerms "" input

-- | Composes parsing and checking that terms are well formed, returns the 
-- result or an error to display.
pureTermParserChecker :: String -> Either String PureTerm
pureTermParserChecker input = first show (pureTermParser input) 
                                >>= locallyClosedCheck

parseAll :: (ParseableTerm ty, ParseableContext ty) => 
                Parser (LambdaTerm ty, Context ty)
parseAll = do
            context <- parseContext
            term <- parseTerms
            return (term, context)

-- | Composes parsing and checking terms, returns the result or an error to 
-- display.
termParserChecker :: String -> Either String (ST.Type, ST.Term)
termParserChecker input = first show (termParser input) >>= 
                            (\x -> first show (uncurry ST.typeCheckAndTerm x))

termParser :: (ParseableTerm ty, ParseableContext ty) => String -> 
                Either ParseError (LambdaTerm ty, Context ty)
termParser input = parse parseAll "" input

