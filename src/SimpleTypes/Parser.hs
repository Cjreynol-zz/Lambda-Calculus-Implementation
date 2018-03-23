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


import              Data.Bifunctor                   (first)
import              Text.Parsec                      (ParseError, char, 
                                                        letter, many, parse, 
                                                        string, (<|>))
import              Text.Parsec.String               (Parser)

import              Context                          (Context)
import              LambdaTerm                       (LambdaTerm(..))
import qualified    Parsing.ParseableContext as PC   (ParseableContext(..))
import              Parsing.ParseableTerm            (ParseableTerm(..))
import              SimpleTypes.Term                 (Term, typeCheckAndTerm)
import              SimpleTypes.Type                 (Type(..))
import              SimpleTypes.TypingError          (ErrorString)


instance ParseableTerm Type where
    parseLam = do
                _ <- string "\\:"
                typ <- parseType -- should this be parseTypes instead?
                _ <- char '.'
                term <- parseTerms
                return $ Lam typ term

instance PC.ParseableContext Type where
    parseTypes = parseTypes

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

parseAll :: Parser (Term,Context Type)
parseAll = do
            context <- PC.parseContext
            term <- parseTerms
            return (term,context)

termParser :: String -> Either ParseError (Term,Context Type)
termParser input = parse parseAll "" input

-- | Composes parsing and checking terms, returns the result or an error to 
-- display.
termParserChecker :: String -> Either ErrorString (Type,Term)
termParserChecker input = first show (termParser input) >>= (\x -> first show (uncurry typeCheckAndTerm x))

