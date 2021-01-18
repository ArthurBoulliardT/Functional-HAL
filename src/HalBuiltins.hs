--
-- EPITECH PROJECT, 2020
-- B-FUN-501-PAR-5-1-HAL-maxence.abela
-- File description:
-- HalBuiltins
--

module HalBuiltins where

import AST
import qualified Env

-- Functions implemented here have to handle their arguments by themselves since they are called by apply, which give them a list of arguments.

atomfalse :: Token
atomfalse = Atom (AtomVal "#f")

atomtrue :: Token
atomtrue = Atom (AtomVal "#t")


isAnAtom :: Token -> Bool
isAnAtom Nil = True
isAnAtom (Atom a) = True
isAnAtom _ = False

isAtomFun :: [Token] -> Maybe Token
isAtomFun [] = Just atomtrue -- An empty list is an atom (Nil), but it should have been converted to a Nil.
isAtomFun [token] = case isAnAtom token of
    True -> Just atomtrue
    False -> Just atomfalse
isAtomFun _ = Nothing

addFun :: [Token] -> Maybe Token
addFun [] = Just (Atom (AtomInt (0)))
addFun (x:xs) = case x of
    Atom (AtomInt n) -> case addFun xs of
        Just (Atom (AtomInt res)) -> Just (Atom (AtomInt (n + res)))
        Nothing -> Nothing
    _ -> Nothing

subFun :: [Token] -> Maybe Token
subFun [] = Just (Atom (AtomInt (0)))
subFun (x:xs) = case x of
    Atom (AtomInt n) -> case subFun xs of
        Just (Atom (AtomInt res)) -> Just (Atom (AtomInt (n - res)))
        Nothing -> Nothing
    _ -> Nothing

mulFun :: [Token] -> Maybe Token
mulFun [] = Just (Atom (AtomInt (1)))
mulFun (x:xs) = case x of
    Atom (AtomInt n) -> case mulFun xs of
        Just (Atom (AtomInt res)) -> Just (Atom (AtomInt (n * res)))
        Nothing -> Nothing
    _ -> Nothing

divFun :: [Token] -> Maybe Token
divFun [] = Just (Atom (AtomInt (1)))
divFun (x:xs) = case x of
    Atom (AtomInt n) -> case divFun xs of
        Just (Atom (AtomInt res)) -> Just (Atom (AtomInt ( div n res)))
        Nothing -> Nothing
    _ -> Nothing

eqFun :: [Token] -> Maybe Token
eqFun [] = Nothing
eqFun [x] = Nothing
eqFun (x1:x2:xs) = case xs of
    [] -> case isAnAtom x1 of
            False -> Just atomfalse
            True -> case isAnAtom x2 of
                False -> Just atomfalse
                True -> case x1 == x2 of
                    False -> Just atomfalse
                    True -> Just atomtrue
    _ -> Nothing

modFun :: [Token] -> Maybe Token
modFun [] = Nothing
modFun [x] = Nothing
modFun (x1@(Atom (AtomInt n1)):x2@(Atom (AtomInt n2)):xs) = Just (Atom (AtomInt (mod n1 n2)))
modFun _ = Nothing

lessThanFun :: [Token] -> Maybe Token
lessThanFun [] = Nothing
lessThanFun [x] = Nothing
lessThanFun (x1:x2:xs) = case xs of
    [] -> case isAnAtom x1 of
            False -> Just atomfalse
            True -> case isAnAtom x2 of
                False -> Just atomfalse
                True -> case x1 < x2 of
                    False -> Just atomfalse
                    True -> Just atomtrue
    _ -> Nothing

moreThanFun :: [Token] -> Maybe Token
moreThanFun [] = Nothing
moreThanFun [x] = Nothing
moreThanFun (x1:x2:xs) = case xs of
    [] -> case isAnAtom x1 of
            False -> Just atomfalse
            True -> case isAnAtom x2 of
                False -> Just atomfalse
                True -> case x1 > x2 of
                    False -> Just atomfalse
                    True -> Just atomtrue
    _ -> Nothing

lessEqualThanFun :: [Token] -> Maybe Token
lessEqualThanFun[] = Nothing
lessEqualThanFun[x] = Nothing
lessEqualThanFun (x1:x2:xs) = case xs of
    [] -> case isAnAtom x1 of
            False -> Just atomfalse
            True -> case isAnAtom x2 of
                False -> Just atomfalse
                True -> case x1 <= x2 of
                    False -> Just atomfalse
                    True -> Just atomtrue
    _ -> Nothing

moreEqualThanFun :: [Token] -> Maybe Token
moreEqualThanFun[] = Nothing
moreEqualThanFun[x] = Nothing
moreEqualThanFun (x1:x2:xs) = case xs of
    [] -> case isAnAtom x1 of
            False -> Just atomfalse
            True -> case isAnAtom x2 of
                False -> Just atomfalse
                True -> case x1 >= x2 of
                    False -> Just atomfalse
                    True -> Just atomtrue
    _ -> Nothing

carFun :: [Token] -> Maybe Token
carFun [] = Nothing
carFun ((List (x:_)):[]) = Just x
carFun ((DottedPair l _):[]) = Just l
carFun _ = Nothing

cdrFun :: [Token] -> Maybe Token
cdrFun [] = Nothing
cdrFun ((List (_:[])):[]) = Just Nil
cdrFun ((List (_:xs)):[]) = Just (List xs)
cdrFun ((DottedPair _ r):[]) = Just r
cdrFun _ = Nothing

consFun :: [Token] -> Maybe Token
consFun (one:[]) = Nothing
consFun (v1:Nil:[]) = Just (List [v1])
consFun (v1:(List a):[]) = Just (List (v1:a))
consFun (v1:v2:[]) = Just (DottedPair v1 v2)
consFun _ = Nothing

quoteFun :: [Token] -> Maybe Token
quoteFun [] = Nothing
quoteFun [anything] = Just (anything)
quoteFun _ = Nothing
