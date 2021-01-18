--
-- EPITECH PROJECT, 2020
-- bootStrap-Hal
-- File description:
-- AST
--

module AST where

import Data.Foldable
import Control.Applicative
import System.Exit
import Parser
import Data.Int

type StringInQuote = String
type UndefinedString = String
type ErrorInfo = String

data AtomType = AtomStr StringInQuote
                | AtomVal UndefinedString
                | AtomInt Data.Int.Int64
                deriving (Show, Eq, Ord)

data Token =    DottedPair { left :: Token, right :: Token}
                | List { content :: [Token] }
                | Atom AtomType
                | Nil
                | Clear
                deriving (Show, Eq, Ord)

-- data Tree = Empty 
--             | Node { left :: Tree
--                     , right :: Tree
--                     , value :: Token
--                     } deriving (Show, Eq)

makeDottedPairFromList :: Token -> Token
makeDottedPairFromList (List []) = Nil
makeDottedPairFromList (List (x:xs)) = DottedPair x $ makeDottedPairFromList (List xs)
makeDottedPairFromList any = any

convertListToDottedPair :: Parser Token -> Parser Token
convertListToDottedPair p = Parser fct
        where   fct [] = Nothing
                fct s = case runParser p s of
                    Nothing -> Nothing
                    Just (rslt, next) -> Just (makeDottedPairFromList rslt, next)

parseList :: Parser Token
parseList = (List) <$> (prefix *>  many (parseSpaced (parseToken)) <* suffix)
        where   prefix = (parseSpaced (parseChar '('))
                suffix =  (parseSpaced (parseChar ')'))

--

parseNil :: Parser Token
parseNil = Parser fct
        where   fct [] = Nothing
                fct ('(':')':xs) = Just (Nil, xs)
                fct _ = Nothing

--

-- VERY IMPORTANT TODO CANNOT START WITH ANYTHING THAT IS NOT ALPHABETICAL, AND SUPPORTS MORE CHARACTERS
parseAtomVal :: Parser AtomType
parseAtomVal = (AtomVal) <$> parseSpaced (some $ parseAnyChar chars)
        where   chars = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "<>=+-*?_#"

parseAtomInteger :: Parser AtomType
parseAtomInteger = (AtomInt) <$> parseSpaced (parseInt)


parseAtomStr :: Parser AtomType
parseAtomStr = (AtomStr) <$> parseQuotedString

parseAtom :: Parser Token
parseAtom = (Atom) <$> (parseAtomInteger <|> parseAtomVal <|> parseAtomStr)

--

parseDottedPair :: Parser Token
parseDottedPair = (DottedPair) <$> (prefix *> parseToken) <*> ( point *> parseToken <* suffix)
        where   prefix = (parseSpaced (parseChar '('))
                point = parseSpaced (parseChar '.')
                suffix =  (parseSpaced (parseChar ')'))

parseQuotedSugarSyntaxt :: Parser Token
parseQuotedSugarSyntaxt = Parser fct
        where   fct [] = Nothing
                fct str = case runParser parseToken str of
                        Nothing -> Nothing
                        Just (rslt, next) -> Just (List [((Atom (AtomVal "quote"))), rslt], next)

parseQuoted :: Parser Token
parseQuoted = prefix *> parseQuotedSugarSyntaxt
        where   prefix = parseSpaced (parseChar '\'')

--

parseToken :: Parser Token
parseToken = parseNil <|> parseAtom <|> parseDottedPair <|> parseList <|> parseQuoted
