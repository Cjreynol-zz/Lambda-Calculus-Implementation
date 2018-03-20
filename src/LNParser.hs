module LNParser(
    lnTermParser
    ) where


import Data.Char            (digitToInt)
import Text.Parsec          (ParseError, char, digit, many1, parse, (<|>))
import Text.Parsec.String   (Parser)

import LNTerm               (LNTerm(..))
import Nat                  (intToNat) 


parseLam :: Parser LNTerm
parseLam = do
            _ <- char '\\'
            _ <- char '.'
            term <- parseLNTerms
            return $ Lam term

parseBVar :: Parser LNTerm
parseBVar = do
            nat <- digit
            return $ BVar (intToNat (digitToInt nat))

parseFVar :: Parser LNTerm
parseFVar = do
                _ <- char 'f'
                nat <- digit
                return $ FVar (intToNat (digitToInt nat))

parseParenLNTerm :: Parser LNTerm
parseParenLNTerm = do
                    _ <- char '('
                    term <- parseLNTerms
                    _ <- char ')'
                    return $ term

parseLNTerm :: Parser LNTerm
parseLNTerm = parseParenLNTerm <|> parseLam <|> parseFVar <|> parseBVar 

parseLNTerms :: Parser LNTerm
parseLNTerms = do
                applications <- many1 parseLNTerm
                return $ foldl1 App applications

-- | Parser for LNTerms.  
-- Need to add check for isLocallyClosed.
lnTermParser :: String -> Either ParseError LNTerm
lnTermParser input = parse parseLNTerms "" input
