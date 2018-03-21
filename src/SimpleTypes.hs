{-|
Module      : SimpleTypes
Description : Simply tymped lambda calculus
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module SimpleTypes(
    Term(..),
    Type(..),
    Context,
    newContext,
    stripTypes,
    typeCheck,
    termParser
    ) where


import SimpleTypes.Context
import SimpleTypes.Parser
import SimpleTypes.Term
import SimpleTypes.Type
