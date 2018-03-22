{-|
Module      : SimpleTypes.Parser
Description : Parsing functions for simply typed lambda calculus terms
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
Description : A typing context is expected first, wrapped in square brackets [].  
                The elements inside are pairs (n,type) where n is a natural 
                number and type is a valid type.  Consecutive pairs do not 
                have any delimiter, and in the case of multiple pairs 
                containing the same number n, the type associated with that 
                variable in the context will be the last in the list.

                Types are expected to be characters, typically uppercase, 
                separated by arrows, ->.

                For terms, no spaces, \\:type. to represent lambda binders.  
                Bound variables are represented as natural numbers 
                referencing their binder as is standard in De Bruijn 
                notation.  Free variables are also represented by natural 
                numbers, but with an 'f' preceding them.  Application is 
                implicit with adjacent terms, and parenthesis can be used to 
                end the binding scope of a lambda.  
-}
module SimpleTypes.Parser(
    termParserChecker
    ) where


import Data.Bifunctor               (first)
import Data.Char                    (digitToInt)
import Text.Parsec                  (ParseError, char, digit, letter, many, 
                                        many1, parse, string, (<|>))
import Text.Parsec.String           (Parser)

import LambdaTerm                   (LambdaTerm(..))
import Nat                          (Atom, intToNat)
import SimpleTypes.Context          (Context, newContext)
import SimpleTypes.Term             (Term, typeCheckAndTerm)
import SimpleTypes.Type             (Type(..))
import SimpleTypes.TypingError      (ErrorString)


parseContext :: Parser Context
parseContext = do
                _ <- char '['
                pairs <- many parseContextEntry
                _ <- string "]|-"
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

parseArrow :: Parser Type
parseArrow = do
                _ <- string "->"
                ty2 <- parseType
                return $ ty2

parseTypes :: Parser Type
parseTypes = do
                typ <- parseType
                types <- many parseArrow
                case types of
                    [] -> return typ
                    _ -> return $ foldr1 Arrow (typ:types)

parseParenType :: Parser Type
parseParenType = do
                    _ <- char '('
                    typ <- parseTypes
                    _ <- char ')'
                    return typ

parseLam :: Parser Term
parseLam = do
            _ <- string "\\:"
            typ <- parseType
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

termParser :: String -> Either ParseError (Term,Context)
termParser input = parse parseAll "" input

-- | Composes parsing and checking terms, returns the result or an error to 
-- display.
termParserChecker :: String -> Either ErrorString (Type,Term)
termParserChecker input = first show (termParser input) >>= (\x -> first show (uncurry typeCheckAndTerm x))

