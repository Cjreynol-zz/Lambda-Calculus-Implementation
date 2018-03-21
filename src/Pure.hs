{-|
Module      : Pure
Description : Pure lambda calculus, implemented using a locally nameless 
                representation for terms
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module Pure(
    Term(..),
    termParser
    ) where


import Pure.Parser
import Pure.Term
