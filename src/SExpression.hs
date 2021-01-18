--
-- EPITECH PROJECT, 2020
-- bootStrap-Hal
-- File description:
-- S_Expression
--

module SExpression where

import Parser
import AST

sExpressionParsing :: Parser Token
sExpressionParsing = parseSpaced (parseChar '\'') *> parseToken

dottedToAbbreviatedString :: Token -> String
dottedToAbbreviatedString (Nil) = "()"
dottedToAbbreviatedString (Atom (AtomStr s)) = "\"" ++ s ++ "\""
dottedToAbbreviatedString (Atom (AtomVal s)) = s
dottedToAbbreviatedString (Atom (AtomInt num)) = show num
dottedToAbbreviatedString (DottedPair l Nil) = (dottedToAbbreviatedString l)
dottedToAbbreviatedString (DottedPair Nil r) = "() . " ++ (dottedToAbbreviatedString r)
dottedToAbbreviatedString (DottedPair l@(DottedPair dL dR) r) = "(" ++ (dottedToAbbreviatedString l) ++ ") . " ++ (dottedToAbbreviatedString r)
dottedToAbbreviatedString (DottedPair l r@(DottedPair dL dR)) = (dottedToAbbreviatedString l) ++ " " ++ (dottedToAbbreviatedString r)
dottedToAbbreviatedString (DottedPair l r) = "(" ++ (dottedToAbbreviatedString l) ++ " . " ++ (dottedToAbbreviatedString r) ++ ")"

complexToStr :: Parser Token -> String -> String
complexToStr p str = case runParser p str of
    Nothing -> ""
    Just (tok, _) -> "(" ++ dottedToAbbreviatedString tok ++ ")"

listToDottedString :: Token -> String
listToDottedString (Nil) = "()"
listToDottedString (Atom (AtomStr s)) = "\"" ++ s ++ "\""
listToDottedString (Atom (AtomVal s)) = s
listToDottedString (Atom (AtomInt num)) = show num
listToDottedString (DottedPair l r) = "(" ++ listToDottedString l ++ " . " ++ listToDottedString r ++ ")"
listToDottedString (List []) = "()"
listToDottedString (List (x:xs)) = "(" ++ (listToDottedString x) ++ " . " ++ (listToDottedString $ List xs) ++ ")"

listToString :: Token -> String
listToString (Nil) = "()"
listToString (Atom (AtomStr s)) = "\"" ++ s ++ "\""
listToString (Atom (AtomVal s)) = s
listToString (Atom (AtomInt num)) = show num
listToString (DottedPair l r) = "(" ++ listToString l ++ " . " ++ listToString r ++ ")"
listToString (List []) = "()"
listToString (List (x:xs)) = "(" ++ (listToString x) ++ (printElemsList xs) ++ ")"

printElemsList :: [Token] -> String
printElemsList [] = ""
printElemsList (x:xs) = " " ++ listToString x ++ printElemsList xs

--
abbreviatedToStr :: Parser Token -> String -> String
abbreviatedToStr p str = case runParser p str of
    Nothing -> ""
    Just (tok, _) -> listToDottedString tok

-- > '( 1 . (2 . ( 3 . ())))
-- (1 2 3)
-- > '( 1 . (2 . ( 3 . 4)))
-- (1 2 3 . 4)
-- > '( () . 3)
-- (() . 3)
-- > '((() . 3) . 2)
-- ((() . 3) . 2)
-- > '( 1 . 4)
-- (1 . 4)
-- > '( 1 . (2 . 3))
-- (1 2 . 3)
-- > '( 1 . (2 . ()))
-- (1 2)
-- > '( (1 . 2). (2 . ()))
-- ((1 . 2) 2)
-- >
