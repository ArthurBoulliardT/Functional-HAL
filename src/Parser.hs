--
-- EPITECH PROJECT, 2020
-- Parser
-- File description:
-- ParserFunctor
--

module Parser where

import Data.Int
import System.Exit
import Control.Applicative
import Text.Read

data  Parser a = Parser 
    { runParser  ::  String -> Maybe (a, String)
    }

-- Fmap :: (a -> b) -> f a -> f b
instance Functor Parser where
    fmap fctWith parser = Parser fct
        where fct str = case runParser parser str of
                    Nothing -> Nothing
                    Just (rslt, next) -> Just $ (fctWith rslt, next)

-- pure :: a -> f a
-- <*> :: f (a -> b) -> f a -> f b
instance Applicative Parser where
    pure a = Parser fct
        where fct str = Just (a, str)

    p1 <*> p2 = Parser fct
        where fct str = case runParser p1 str of
                    Nothing -> Nothing
                    Just (fctUnWrapped, next1) -> case runParser p2 next1 of
                        Nothing -> Nothing
                        Just (rslt, next2) -> Just $ (fctUnWrapped rslt, next2)

-- empty :: f a
-- (<|>) :: f a -> f a -> f a
instance Alternative Parser where
    empty = Parser fct
        where fct str = Nothing

    p1 <|> p2 = Parser fct
        where fct str = case runParser p1 str of
                Nothing -> runParser p2 str
                Just (rslt, next) -> Just $ (rslt, next)


-- return :: a -> m a
-- bind :: (>>=) :: m a -> (a -> m b) -> m b

instance Monad Parser where
    return = pure
    p1 >>= fctWith = Parser fct
        where fct str = case runParser p1 str of
                Nothing -> Nothing
                Just (rslt1, next1) -> runParser (fctWith rslt1) next1

-- subtilités pour opérateurs de même priorité (gérer tout le reste d'abord)
-- petite subtilité pour la gérer
-- problème sur le - et le /

--

parseChar :: Char -> Parser Char
parseChar n = Parser fct
    where   fct [] = Nothing
            fct (x:xs)  | n == x = Just (x, xs)
                        | otherwise = Nothing

--

parseAnyChar ::  String -> Parser Char
parseAnyChar str = Parser fct
    where   fct [] = Nothing
            fct (x:xs) = if elem x str
                    then Just (x, xs)
                    else Nothing

--

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = p1 <|> p2

-- -- 

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd = parseAndWith (,)

--

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith fctWith p1 p2 = (fctWith <$> p1) <*> p2

-- --

readAnInt :: [Char] -> Data.Int.Int64
readAnInt [] = 0
readAnInt str = read str::Data.Int.Int64

parseAndGetUInt :: Parser String
parseAndGetUInt = (some (parseAnyChar "0123456789")) <* parseSpace

parseUInt :: Parser Data.Int.Int64
parseUInt = (readAnInt) <$> parseAndGetUInt

-- --

--readANegInt :: [Char] -> Int
--readANegInt [] = 0
--readANegInt str = (-1 * read str::Int)

parseAndGetInt :: Parser String
parseAndGetInt = (++) <$> (parseSpace *> many (parseChar '-')) <*> parseAndGetUInt

parseInt :: Parser Data.Int.Int64
parseInt = (readAnInt) <$> parseAndGetInt

--

parseSpace :: Parser String
parseSpace = (many (parseAnyChar " \n\t"))

parseSpaced :: Parser a -> Parser a
parseSpaced p = parseSpace *> p <* parseSpace

parseComma :: Parser Char
parseComma = (parseSpaced (parseChar ','))

parsePoint :: Parser Char
parsePoint = (parseSpaced (parseChar '.'))

parseOpenPar :: Parser Char
parseOpenPar = (parseSpaced (parseChar '('))

parseClosePar :: Parser Char
parseClosePar = (parseSpaced (parseChar ')'))

parseDoubleQuote :: Parser Char
parseDoubleQuote = (parseSpaced (parseChar '\"'))

parseSimpleQuote :: Parser Char
parseSimpleQuote = (parseSpaced (parseChar '\''))

parseTuple :: Parser a -> Parser (a, a)
parseTuple p1 = (,) <$> (parseOpenPar *> p1) <*> (parseComma *> p1 <* parseClosePar)

--

readADouble :: [Char] -> Double
readADouble [] = 0
readADouble str = read str::Double

parseAndGetFloat :: Parser String
parseAndGetFloat = (++) <$> (parseSpace *> parseAndGetInt) <*> (parseAndWith (:) parsePoint parseAndGetUInt)

parseDouble :: Parser Double
parseDouble = (readADouble) <$> (parseAndGetFloat <|> parseAndGetInt)

--

type Count = Int
type BasicString = String
type CopyString = String
type BasicStringDecaleCopy = String

getStringFromWithParentheses :: Count -> String -> String
getStringFromWithParentheses _ [] = []
getStringFromWithParentheses 0 _ = []
getStringFromWithParentheses 1 (')':_) = []
getStringFromWithParentheses c ('(':xs) = '(' : getStringFromWithParentheses (1+c) xs
getStringFromWithParentheses c (')':xs) = ')' : getStringFromWithParentheses ((-1)+c) xs
getStringFromWithParentheses c (x:xs) = x : getStringFromWithParentheses (c) xs

decaleStringFrom :: BasicString -> CopyString -> BasicStringDecaleCopy
decaleStringFrom [] cStr = cStr
decaleStringFrom bStr [] = bStr
decaleStringFrom (_:bStrXs) (_:cStrXs) = decaleStringFrom bStrXs cStrXs

parseParentheses :: Parser a -> Parser a
parseParentheses p1 = (parseOpenPar *> p1 <* parseClosePar)

parseAllSinceClosePar :: Parser String
parseAllSinceClosePar = Parser fct
    where   fct [] = Nothing
            fct str@(x:xs) = case getStringFromWithParentheses 1 str of
                ")" -> Just $ ([x], xs)
                rslt -> Just $ (rslt, (decaleStringFrom str rslt))

parsePairParentheses :: Parser String
parsePairParentheses = parseOpenPar *> parseAllSinceClosePar <* parseClosePar

--

getQuotedStringContent :: String -> String
getQuotedStringContent [] = []
getQuotedStringContent ('\\':'\"':xs) = '\\' : '\"' : getQuotedStringContent xs
getQuotedStringContent ('\"':_) = []
getQuotedStringContent (x:xs) = x : getQuotedStringContent xs

parseContentQuotedString :: Parser String
parseContentQuotedString = Parser fct
        where   fct [] = Nothing
                fct str@(x:xs) = case getQuotedStringContent str of
                    "\"" -> Just $ ([x], xs)
                    rslt -> Just (rslt, (decaleStringFrom str rslt))

parseQuotedString :: Parser String
parseQuotedString = parseDoubleQuote *> parseContentQuotedString <* parseDoubleQuote

--

parseUnary :: Parser Char
parseUnary = (parseSpaced (parseChar '-'))

--
