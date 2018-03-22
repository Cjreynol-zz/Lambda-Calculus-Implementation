{-|
Module      : SimpleTypes.TypingError
Description : Type of that can arise in type checking and parsing
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module SimpleTypes.TypingError(
    TypingError(..),
    ParseOrTypeError,
    convertP,
    convertT
    ) where


import Text.Parsec (ParseError)


-- | Represents errors during type checking
data TypingError = TyErr String

-- | Intended to be a more abstract version of each of the errors
data ParseOrTypeError = TypError String | ParError String

-- | Converts to the more general error type
convertP :: ParseError -> ParseOrTypeError
convertP p = ParError $ show p

-- | Converts to the more general error type
convertT :: TypingError -> ParseOrTypeError
convertT (TyErr t) = TypError t

instance Show ParseOrTypeError where
    show (TypError s) = show s
    show (ParError s) = show s

