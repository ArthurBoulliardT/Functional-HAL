--
-- EPITECH PROJECT, 2020
-- B-FUN-501-PAR-5-1-HAL-maxence.abela
-- File description:
-- Spec
--

import EvalAndApply

main :: IO ()
main = putStrLn "Test suite not yet implemented"

testApplyLambda :: Maybe Token
testApplyLambda = applyLambda 
        (List [ (Atom (AtomVal "lambda")),
                (List [ (Atom (AtomVal "a")),
                        (Atom (AtomVal "b"))
                        ]),
                (List [(Atom (AtomVal "+")),
                        (Atom (AtomVal "a")),
                        (Atom (AtomVal "b"))
                        ])])
        (List [(Atom (AtomInt 1)), 
                (Atom (AtomInt 2))
                ])
        Env.empty

testApply :: Maybe Token
testApply = apply 
        (List [ (Atom (AtomVal "lambda")),
                (List [ (Atom (AtomVal "a")),
                        (Atom (AtomVal "b"))
                        ]),
                (List [(Atom (AtomVal "+")),
                        (Atom (AtomVal "a")),
                        (Atom (AtomVal "b"))
                        ])])
        (List [(Atom (AtomInt 1)), 
                (Atom (AtomInt 2))
                ])
        Env.empty

testApplyWithUserFct :: Maybe Token
testApplyWithUserFct = apply (Atom (AtomVal "testFunction")) (List [(Atom (AtomInt 1)), (Atom (AtomInt 2))]) $ Env.insert (AtomVal "testFunction") (List [ (Atom (AtomVal "lambda")),(List [ (Atom (AtomVal "a")),(Atom (AtomVal "b"))]),(List [(Atom (AtomVal "+")),(Atom (AtomVal "a")),(Atom (AtomVal "b"))])]) Env.empty

--

loopContent :: String -> IO ExitCode
loopContent [] = return $ ExitSuccess
loopContent content = case runParser parseToken content of
        Nothing -> return $ ExitFailure 84
        Just (rslt, next) -> printf "%s\n" (show rslt) >> loopContent next


testFileTokenization :: String -> IO ExitCode
testFileTokenization [] = return $ ExitFailure 84
testFileTokenization str = catch (readFile str) expFct
            >>= \ strOrExc -> case strOrExc of
                    "" -> return $ ExitFailure 84
                    content -> printf "%s\n" (content) >> loopContent content
    where expFct = (\e -> hPutStr stderr ("Warning: Couldn't open " ++ str ++ ": " ++ (show (e :: IOException)) ++ "\n") >> return "")


--

testListPrint :: String -> String
testListPrint [] = ""
testListPrint str = case runParser parseToken str of
        Nothing -> ""
        Just (rslt, next) -> listToString rslt
