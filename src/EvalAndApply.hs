--
-- EPITECH PROJECT, 2020
-- B-FUN-501-PAR-5-1-HAL-maxence.abela
-- File description:
-- Apply
--

module EvalAndApply where

import HalBuiltins
import AST
import qualified Env

findFun :: String -> Maybe ([Token] -> Maybe Token)
findFun "+" = Just addFun
findFun "-" = Just subFun
findFun "*" = Just mulFun
findFun "div" = Just divFun
findFun "mod" = Just modFun
findFun "<" = Just lessThanFun
findFun ">" = Just moreThanFun
findFun "<=" = Just lessEqualThanFun
findFun ">=" = Just moreEqualThanFun
findFun "eq?" = Just eqFun
findFun "atom?" = Just isAtomFun
findFun "car" = Just carFun
findFun "cdr" = Just cdrFun
findFun "cons" = Just consFun
findFun _ = Nothing

--

buildLambdaEnv :: [Token] -> [Token] -> Env.EnvType -> Maybe Env.EnvType
buildLambdaEnv [] [] env = Just env
buildLambdaEnv [] _ _ = Nothing
buildLambdaEnv _ [] _ = Nothing
buildLambdaEnv (name:xs1) (value:xs2) env = case name of
        (Atom (AtomVal n)) -> buildLambdaEnv xs1 xs2 (Env.insert (AtomVal n) value env)
        _ -> Nothing

-- ((lambda (a b c) (code)) a b c)
-- TODO check lambda at definition, before it gets called.
-- The idea is to call eval on (code) after adding args to the environment that is passed to this eval.
-- Since it needs eval, we may have to put it in the same file as eval.

applyLambda :: Token -> Token -> Env.EnvType -> Maybe Token
applyLambda (List []) _ _ = Nothing
applyLambda (List (x1:[])) _ _ = Nothing
applyLambda (List (x1:x2:[])) _ _ = Nothing
applyLambda (List ((Atom (AtomVal "lambda")):(List argsName):x3:[])) (List argsVal) env = case buildLambdaEnv argsName argsVal env of
        Just lambda_env -> fst $ eval x3 lambda_env
        Nothing -> Nothing
applyLambda _ _ _ = Nothing

--

evalArgList :: [Token] -> Env.EnvType -> Maybe [Token]
evalArgList [] _ = Just []
evalArgList (x:xs) env = case eval x env of
        (Nothing, _) -> Nothing
        (Just token, env) -> case evalArgList xs env of
                Nothing -> Nothing
                Just tokens -> Just (token : tokens)

--

-- plusieurs cas
-- Si :
-- - 1er paramètre List et 2eme Paramètre List -> apply Lambda
-- - 1er paramètre Atom -> fct ou erreur -> Dans le cas de fct -> qu'est ce qu'on fait entre impl de l'utilisateur ou les builtins
-- 


applyUserDefinedFct :: Token -> Token -> Env.EnvType -> Maybe Token
applyUserDefinedFct fctExpr (List args) env = case evalArgList args env of
                Nothing -> Nothing
                Just argsEval -> applyLambda fctExpr (List argsEval) env

applyBuiltInFct :: Token -> Token -> Env.EnvType -> Maybe Token
applyBuiltInFct (Atom (AtomVal fctName)) (List args) env = case findFun fctName of
                Nothing -> Nothing
                Just fun -> case evalArgList args env of
                        Nothing -> Nothing
                        Just argsEval -> fun argsEval

applyFunction :: Token -> Token -> Env.EnvType -> Maybe Token
applyFunction tknName@(Atom fctName) args env = case Env.lookup fctName env of
        Nothing -> applyBuiltInFct tknName args env
        Just fctExpr@(List tokens) -> applyUserDefinedFct fctExpr args env
        Just (_) -> Nothing --handle dottedPairs here later.


apply :: Token -> Token -> Env.EnvType -> Maybe Token
apply lbdFct@(List _) lbdArgs@(List _) env = applyLambda lbdFct lbdArgs env
apply aFct@(Atom (AtomVal _)) args@(List _) env = applyFunction aFct args env
apply _ _ _ = Nothing

applyDefine :: Token -> Token -> Env.EnvType -> Maybe Env.EnvType
applyDefine (Atom (AtomVal name)) (List []) env = Nothing
applyDefine (Atom (AtomVal name)) (List (args:[])) env = case args of
        List ((Atom (AtomVal "lambda")):xs) -> Just (Env.insert (AtomVal name) args env)
        _ -> case fst $ eval args env of
                Just value -> Just (Env.insert (AtomVal name) value env)
                Nothing -> Nothing
applyDefine (List ((Atom name@(AtomVal n)):args)) (List (code:[])) env =
        Just (Env.insert name (List ((Atom (AtomVal "lambda")):(List args):code:[])) env)
applyDefine _ _ env = Nothing

applyCond :: Token -> Env.EnvType -> Maybe Token
applyCond (List []) _ = Nothing
applyCond (List ((List (cond:value:[])):xs)) env = case fst $ eval cond env of
        Just (Atom (AtomVal "#f")) -> applyCond (List xs) env
        Just (Atom (AtomVal "#t")) -> fst $ eval value env
        _ -> Nothing
applyCond _ _ = Nothing

-- (let ((n v)) (body))
applyLet :: Token -> Env.EnvType -> Maybe Token
applyLet (List []) _ = Nothing
applyLet (List (_:[])) _ = Nothing
applyLet (List ((List []):(List expression):[])) env = fst $ eval (List expression) env
applyLet (List ((List ((List [(Atom name@(AtomVal _)), value]):xs)):(List expression):[])) env = 
        applyLet (List [(List xs), List expression]) $ Env.insert name value env
applyLet _ _ = Nothing

-- evaluates a list of tokens.
-- Paul Graham's document doesn't use apply but Scheme does, so we'll do it the same way as scheme does,
-- which may even be easier and more readable.

eval :: Token -> Env.EnvType -> (Maybe Token, Env.EnvType)
eval true@(Atom (AtomVal "#t")) env = (Just true, env)
eval false@(Atom (AtomVal "#f")) env = (Just false, env)
eval (Atom a@(AtomVal s)) env = case Env.lookup a env of
        Just res -> (Just res, env)
        Nothing -> case findFun s of
                Just _ -> (Just (Atom a), env)
                Nothing -> (Nothing, env)
eval a@(Atom _) env = (Just a, env)
eval Nil env = (Just Nil, env)
eval (DottedPair l r) env = (apply l r env, env)
eval (List []) env = (Nothing, env)
eval (List (x1:[])) env = (Nothing, env)
eval l@(List ((Atom (AtomVal "lambda")):xs)) env = (Just l, env)
eval (List ((Atom (AtomVal "define")):x2:xs)) env = case applyDefine x2 (List xs) env of
        Just (new_env) -> (Just Clear, new_env)
        Nothing -> (Just (List xs), env)
eval (List ((Atom (AtomVal "let")):xs)) env = (applyLet (List xs) env, env)
eval (List ((Atom (AtomVal "quote")):xs)) env = (quoteFun xs, env)
eval (List ((Atom (AtomVal "cond")):xs)) env = (applyCond (List xs) env, env)
eval (List (x:xs)) env = (apply x (List (xs)) env, env)
eval _ env = (Nothing, env)
