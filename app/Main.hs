--
-- EPITECH PROJECT, 2020
-- Hal
-- File description:
-- Main
--

module Main where

import Parser
import Interpreter
import Errors
import System.Exit
import System.Environment
import qualified Env

main :: IO ()
main =  do  args <- getArgs
            exitstatus <- isErrors args
            case exitstatus of
                ExitSuccess ->  launchProgram args >>= \ exitstatusLaunching ->
                                exitWith exitstatusLaunching
                _ -> exitWith exitstatus

launchProgram :: [String] -> IO ExitCode
launchProgram [] = return $ ExitSuccess
launchProgram fileNames = interpreter fileNames Env.empty False
