--
-- EPITECH PROJECT, 2020
-- B-FUN-501-PAR-5-1-HAL-maxence.abela
-- File description:
-- Env
--

module Env
    ( module Env
    , module Data.Map
    ) where

import AST
import Data.Map

type EnvType = Map AtomType Token
