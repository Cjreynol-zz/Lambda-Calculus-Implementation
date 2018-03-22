{-|
Module      : SimpleTypes.TypingError
Description : Type of that can arise in type checking and parsing
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module SimpleTypes.TypingError(
    TypingError(..),
    ErrorString
    ) where


-- | Represents errors during type checking.
data TypingError = TyErr String

instance Show TypingError where
    show (TyErr s) = s

-- | Used as a container for parsing and typing errors when the operations 
-- are composed.
type ErrorString = String

