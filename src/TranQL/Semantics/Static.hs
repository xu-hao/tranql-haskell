module TranQL.Semantics.Static where

import TranQL.Syntax
import Data.List (find)
import Control.Monad.Trans.Except

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
        App e f -> do
            s <- typeCheck tenv e
            t <- typeCheck tenv f
            case s of
                TFun q r -> 
                    if q == t
                        then return r
                        else case q of
                            TRel p ->
                                if p == t 
                                    then return r
                                    else fail ("type mismatch: expression " ++ show f ++ ", expected " ++ show q ++ " or " ++ show p ++ ", encounterd " ++ show t)
                            TFloat ->
                                if t == TInteger
                                    then return r
                                    else fail ("type mismatch: expression " ++ show f ++ ", expected " ++ show q ++ " or integer, encounterd " ++ show t)
                            _ ->
                                fail ("type mismatch: expression " ++ show f ++ ", expected " ++ show q ++ ", encounterd " ++ show t)
                _ -> fail ("type mismatch: the type of expression " ++ show f ++ " " ++ show s ++ " is not a function type")
        Abs v s e -> do
            t <- typeCheck ((v, s) : tenv) e
            return (TFun s t)
        Let v e f -> do
            t <- typeCheck tenv e
            typeCheck ((v, t) : tenv) f
        Fresh v t f ->
            typeCheck ((v, TRel t) : tenv) f
        Select selectors e -> do
            let fields = map (\(Selector _ f) -> f) selectors
                exprs = map (\(Selector e _) -> e) selectors
            ts <- mapM (typeCheck tenv) exprs
            s <- typeCheck tenv e
            if s == TProp
                then return (TRecord (zipWith TField fields ts))
                else fail ("type mismatch: the type of expression " ++ show e ++ " " ++ show s ++ " is not prop")
        Dot e f -> do
            s <- typeCheck tenv e
            case s of
                TRecord tfields ->
                    case find (\(TField g t) -> g == f) tfields of
                        Nothing -> fail ("type mismatch: the type of expression " ++ show e ++ " " ++ show s ++ " is a record type, but does not have field " ++ show f)
                        Just (TField _ t) -> return t
                _ -> fail ("type mismatch: the type of expression " ++ show e ++ " " ++ show s ++ " is not a record type")


