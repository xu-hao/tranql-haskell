module TranQL.Semantics.Static where

import TranQL.Syntax
import Data.List (foldl', find, nub, (\\))
import Control.Monad.Trans.Except
import Control.Monad (foldM)

type TEnv = [ (V, T) ]

lookupTEnv :: V -> TEnv -> Maybe T
lookupTEnv _ [] = Nothing
lookupTEnv u ((v, t) : renv) | u == v = Just t
                             | otherwise = lookupTEnv u renv

typeCheck :: TEnv -> Expr -> Except String T
typeCheck tenv e =
    case e of
        IntegerConst _ -> return TInteger
        StringConst _ -> return TString
        FloatConst _ -> return TFloat
        Var v -> case lookupTEnv v tenv of
                        Nothing -> fail ("unbound variable " ++ show v)
                        Just t -> return t
        Abs v s e -> do
            TFun s <$> typeCheck ((v, s) : tenv) e
        App e f -> do
            s <- typeCheck tenv e
            case s of
                TFun q r -> do
                    t <- typeCheck tenv f
                    if q == t
                        then return r
                        else fail ("type mismatch: expression " ++ show f ++ ", expected " ++ show q ++ ", encounterd " ++ show t)
                _ -> fail ("type mismatch: the type of expression " ++ show f ++ " " ++ show s ++ " is not a function type")
        Let v e f -> do
            t <- typeCheck tenv e
            typeCheck ((v, t) : tenv) f
        Fresh v t f ->
            typeCheck ((v, TRel t) : tenv) f
        Return e ->
            TRel <$> typeCheck tenv e
        Select selectors e -> do
            let fields = map (\(Selector _ f) -> f) selectors
                exprs = map (\(Selector e _) -> e) selectors
            ts <- mapM (typeCheck tenv) exprs
            s <- typeCheck tenv e
            if s == TProp
                then return (TRecord (zipWith TField fields ts))
                else fail ("type mismatch: the type of expression " ++ show e ++ " " ++ show s ++ " is not prop")
        From v e f -> do
            s <- typeCheck tenv e
            case s of
                TSet s0 ->
                    typeCheck ((v, s0) : tenv) f
                _ -> fail ("type mismatch: the type of expression " ++ show e ++ " " ++ show s ++ " is not a set type")
        Dot e f -> do
            s <- typeCheck tenv e
            case s of
                TRecord tfields ->
                    case find (\(TField g t) -> g == f) tfields of
                        Nothing -> fail ("type mismatch: the type of expression " ++ show e ++ " " ++ show s ++ " is a record type, but does not have field " ++ show f)
                        Just (TField _ t) -> return t
                _ -> fail ("type mismatch: the type of expression " ++ show e ++ " " ++ show s ++ " is not a record type")


infer :: TEnv -> TEnv -> Maybe T -> Expr -> Except String (Expr, [(V, T)], T)
infer tenv vts expectedType e = do
    r'@(e', vts', t') <- case e of
        IntegerConst _ -> return (e, vts, TInteger)
        StringConst _ -> return (e, vts, TString)
        FloatConst _ -> return (e, vts, TFloat)
        Var v -> case lookupTEnv v tenv of
                        Nothing -> case lookupTEnv v vts of
                            Nothing -> case expectedType of
                                Nothing -> fail ("unbound variable " ++ show v)
                                Just t2 -> return (e, (v, t2) : vts, t2)
                            Just t -> return (e, vts, t)
                        Just t -> return (e, vts, t)
        Abs v s e -> do
            (e', vts', t) <- infer ((v, s) : tenv) vts Nothing e -- must ensure v shadows vts in Var v
            return (Abs v s e', vts', TFun s t)
        App e f -> do
            (e', vts', s) <- infer tenv vts Nothing e
            case s of
                TFun q r -> do
                    (f', vts'', _) <- infer tenv vts' (Just q) f
                    return (App e' f', vts'', r)
                _ -> fail ("type mismatch: the type of expression " ++ show f ++ " " ++ show s ++ " is not a function type")
        Let v e f -> do
            (e', vts', t) <- infer tenv vts Nothing e
            (f', vts'', s) <- infer ((v, t) : tenv) vts' Nothing f
            return (Let v e' f', vts'', s)
        Fresh v t f ->
            infer ((v, TRel t) : tenv) vts Nothing f -- same as Abs v s e
        Return e -> do
            (e', vts', t) <- infer tenv vts Nothing e
            return (Return e', vts', t)
        Select selectors e -> do
            (e', vtss, s) <- infer tenv [] (Just TProp) e -- no free var from outer scope
            let fields = map (\(Selector _ f) -> f) selectors
                exprs = map (\(Selector e _) -> e) selectors
            (exprs', ts) <- foldM (\(exprst, tst) expr -> do
                (expr', vtss', t) <- infer tenv vtss Nothing expr
                if length vtss' /= length vtss 
                    then fail ("unbound variables in selector " ++ show (vtss' \\ vtss))
                    else return (exprst ++ [expr'], tst ++ [t])
                ) ([], []) exprs
            return (foldl' (\et (v, t) -> Fresh v t et) (Select (zipWith Selector exprs' fields) e') vtss, vts, TRecord (zipWith TField fields ts))
        From v e f -> do
            (e', vts', s) <- infer tenv vts Nothing e
            case s of
                TSet s0 -> do
                    (f', vts'', t) <- infer ((v, s0) : tenv) vts' Nothing f
                    return (From v e' f', vts'', t)
                _ -> fail ("type mismatch: the type of expression " ++ show e ++ " " ++ show s ++ " is not a set type")
        Dot e f -> do
            (e', vts', s) <- infer tenv vts Nothing e
            case s of
                TRecord tfields ->
                    case find (\(TField g t) -> g == f) tfields of
                        Nothing -> fail ("type mismatch: the type of expression " ++ show e ++ " " ++ show s ++ " is a record type, but does not have field " ++ show f)
                        Just (TField _ t) -> return (Dot e' f, vts', s)
                _ -> fail ("type mismatch: the type of expression " ++ show e ++ " " ++ show s ++ " is not a record type")
    case expectedType of
        Just t2 | t' == t2 -> return (e', vts', expectedType)
        Just (TRel t2) | t' == t2 -> return (e', vts', expectedType)
        Just _ -> fail ("type mismatch: expression " ++ show e ++ ", expected " ++ show t2 ++ ", encounterd " ++ show t')
        _ -> return r'
