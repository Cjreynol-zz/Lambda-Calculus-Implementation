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
import Text.Parsec              (ParseError, char, parse)

import LambdaTerm               (LambdaTerm(..))
import Parsing.ParseableTerm    (ParseableTerm(..))
import Pure.Term                (Term, locallyClosedCheck)
import SimpleTypes.TypingError  (ErrorString)


instance ParseableTerm () where
    parseLam = do
                _ <- char '\\'
                _ <- char '.'
                term <- parseTerms
                return $ Lam () term

termParser :: String -> Either ParseError Term
termParser input = parse parseTerms "" input

-- | Composes parsing and checking that terms are well formed, returns the 
-- result or an error to display.
termParserChecker :: String -> Either ErrorString Term
termParserChecker input = first show (termParser input) >>= locallyClosedCheck
