--
-- EPITECH PROJECT, 2020
-- funEvalExpr
-- File description:
-- Errors
--

module Errors
    (isErrors
    ) where

import System.Exit
import System.IO
import Control.Applicative
import Data.Char

size :: [String] -> Int
size [] = 0
size (_:xs) = 1 + size xs

isErrors :: [String] -> IO ExitCode
isErrors [] = return $ ExitFailure 84
isErrors str = return $ ExitSuccess