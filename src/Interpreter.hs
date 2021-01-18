--
-- EPITECH PROJECT, 2020
-- B-FUN-501-PAR-5-1-HAL-maxence.abela
-- File description:
-- Interpreter
--

module Interpreter 
    ( interpreter
    ) where

import System.Exit
import System.IO
import Control.Applicative
import Text.Printf
import AST
import EvalAndApply
import Parser
import Control.Exception
import qualified Env
import SExpression
import Prelude

--

countNbParentheses :: String -> Int
countNbParentheses [] = 0
countNbParentheses ('(':xs) = 1 + countNbParentheses xs
countNbParentheses (')':xs) = (-1) + countNbParentheses xs
countNbParentheses (x:xs) = countNbParentheses xs

checkParentheses :: String -> IO ExitCode
checkParentheses [] = return $ ExitFailure 84
checkParentheses str = case countNbParentheses str of
                    0 -> return $ ExitSuccess
                    _ -> return $ ExitFailure 84

type ContentFile = String
type FileName = String
type FileNames = [String]

dealWithInterpretation :: (Maybe Token, Env.EnvType) -> IO (Maybe Env.EnvType)
dealWithInterpretation (Nothing, _) = return Nothing
dealWithInterpretation (Just Clear, env) = return $ Just env
dealWithInterpretation (Just lTkn@(List _), env) = printf "%s\n" (listToString lTkn) >>= \ _ -> return $ Just env
dealWithInterpretation (Just dTkn@(DottedPair _ _), env) = printf "%s\n" (dottedToAbbreviatedString dTkn) >>= \ _ -> return $ Just env
dealWithInterpretation (Just iTkn@(Atom (AtomInt _)), env) = printf "%s\n" (listToString iTkn) >>= \ _ -> return $ Just env
dealWithInterpretation (Just vTkn@(Atom (AtomVal _)), env) = printf "%s\n" (listToString vTkn) >>= \ _ -> return $ Just env
dealWithInterpretation (Just sTkn@(Atom (AtomStr _)), env) = printf "%s\n" (listToString sTkn) >>= \ _ -> return $ Just env
dealWithInterpretation _ = return Nothing

loopTheInterpretation :: ContentFile -> Maybe Env.EnvType -> IO (Maybe Env.EnvType)
loopTheInterpretation [] _ = return Nothing
loopTheInterpretation _ Nothing = return Nothing
loopTheInterpretation content (Just env) = interpreteAFile content env

interpreteAFile :: ContentFile -> Env.EnvType -> IO (Maybe Env.EnvType)
interpreteAFile [] env = return Nothing
interpreteAFile str env = case runParser parseToken str of
                    Nothing -> return Nothing
                    Just (rslt, []) -> dealWithInterpretation $ eval rslt env -- eval result
                    Just (rslt, next) ->  dealWithInterpretation (eval rslt env) >>= \ rslt -> loopTheInterpretation next rslt

interpreter :: FileNames -> Env.EnvType -> Bool -> IO ExitCode -- Env
interpreter [] env True = printf "Launching Interactive Mode\nYou can start writing!\n" >>= \_ -> interactiveMode env
interpreter [] _ _= return ExitSuccess
interpreter ("-i":xs) env _ = interpreter xs env True
interpreter (x:xs) env state = catch (readFile x) expFct
                >>= \ strOrExc -> case strOrExc of
                    "" -> return $ ExitFailure 84
                    content -> checkParentheses content 
                        >>= \ exitStatus -> case exitStatus of 
                            ExitFailure 84 -> return $ ExitFailure 84
                            _ -> interpreteAFile content env >>= \ rslt -> case rslt of
                                                                Nothing -> return $ ExitFailure 84
                                                                Just newEnv -> interpreter xs newEnv state
    where expFct = (\e -> hPutStr stderr ("Warning: Couldn't open " ++ x ++ ": " ++ (show (e :: IOException)) ++ "\n") >> return "")



-- Bonus --

interactiveMode :: Env.EnvType -> IO ExitCode
interactiveMode env = printf "<Ask>\n" >>= \ _ ->
        getLine >>= \ content -> case content of 
        "exit" -> return ExitSuccess
        "clean" -> printf "Cleaning the environment \n" >>= \ _ -> interactiveMode Env.empty
        _ -> printf "<Response>\n" >>= \ _ -> checkParentheses content >>= \ exitStatus -> case exitStatus of
                    ExitFailure 84 -> return $ ExitFailure 84
                    _ -> interpreteAFile content env >>= \ rslt -> case rslt of
                            Nothing -> printf "error\n" >>= \ _ -> interactiveMode env
                            Just newEnv -> interactiveMode newEnv