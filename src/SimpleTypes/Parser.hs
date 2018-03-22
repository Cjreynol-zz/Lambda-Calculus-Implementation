{-|
Module      : SimpleTypes.Parser
Description : Parsing functions for simply typed lambda calculus terms
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module SimpleTypes.Parser(
    termParser
    ) where


import Data.Char                    (digitToInt)
import Text.Parsec                  (char, digit, letter, many, many1, parse, 
                                        (<|>))
import Text.Parsec.String           (Parser)

import Nat                          (Atom, intToNat)
import qualified Pure.Term  as Pure (Term)
import SimpleTypes.Context          (Context, newContext)
import SimpleTypes.Term             (Term(..), typeCheckAndRemove)
import SimpleTypes.Type             (Type(..))
import SimpleTypes.TypingError      (ParseOrTypeError, convertP, convertT)


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

parseArrow :: Parser Type
parseArrow = do
                _ <- char '-'
                _ <- char '>'
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
            _ <- char '\\'
            _ <- char ':'
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

-- | Parser for Terms.  
--
-- A typing context is expected first, wrapped in square brackets [].  
-- The elements inside are pairs (n,type) where n is a natural number and 
-- type is a valid type.  Consecutive pairs do not have any delimiter, and 
-- in the case of multiple pairs containing the same number n, the type 
-- associated with that variable in the context will be the last in the list.
--
-- Types are expected to be characters, typically uppercase, separated by 
-- arrows, ->.
--
-- For terms, no spaces, \\ . to represent lambda binders.  Bound variables 
-- are represented as natural numbers referencing their binder as is standard 
-- in De Bruijn notation.  Free variables are also represented by natural 
-- numbers, but with an 'f' preceding them.  Application is implicit with 
-- adjacent terms, and parenthesis can be used to end the binding scope of a 
-- lambda.  
-- 
-- For example:  
-- 
-- []|-\\:T.0 is equivalent to (\\.(0)) checked in an empty context.
--
-- [(0,S->T)(1,S)]|-f0f1 is equivalent to (0 1) checked in the context where
-- 0 is a function from S to T and 1 is of type S.
termParser :: String -> Either ParseOrTypeError (Type,Pure.Term)
termParser input = 
    case parseResult of
        (Right (t,c)) -> 
            let tcResult = typeCheckAndRemove t c in
            case tcResult of 
                (Right ty) -> Right ty
                (Left err) -> Left $ convertT err
        (Left err) -> Left $ convertP err
    where
        parseResult = parse parseAll "" input
