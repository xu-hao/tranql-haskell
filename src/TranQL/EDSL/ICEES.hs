{-# LANGUAGE RankNTypes, DataKinds, KindSignatures, PolyKinds, TypeFamilies, ScopedTypeVariables, TypeApplications, GADTs #-}
module TranQL.EDSL.ICEES where

import TranQL.EDSL.Query

-- example ICEES query subsystem
data IFeature where
    AgeStudyStart :: IFeature
    AvgDailyPM25Exposure :: IFeature
    Theophylline :: IFeature

data IOp (f :: IFeature) where
    Le :: forall (f :: IFeature). IOp f
    Ge :: forall (f :: IFeature). IOp f
    Lt :: forall (f :: IFeature). IOp f
    Gt :: forall (f :: IFeature). IOp f
    Eq :: forall (f :: IFeature). IOp f
    Ne :: forall (f :: IFeature). IOp f

data AgeBins = A0_2
    | A3_17
    | A18_34
    | A35_50
    | A51_69
    | A70_89

data Quintile = Q1
    | Q2
    | Q3
    | Q4
    | Q5

type family IValue (f :: IFeature) :: * where
    IValue AgeStudyStart = AgeBins
    IValue AvgDailyPM25Exposure = Quintile
    IValue Theophylline = Bool

data IProp where
    ITrue :: IProp
    IAnd :: IProp -> IProp -> IProp
    ICond :: forall (f :: IFeature). IOp f -> IValue f -> IProp

data ICEES = ICEES

data ISelector = Patient

instance PreQuery ICEES where
    type Selector ICEES = ISelector
    type Prop ICEES = IProp

instance Query ICEES where
    data ResultSet ICEES _ = ICohort String
    exec = undefined

associationsToAllFeatures :: forall (f :: IFeature) (a :: Selector ICEES). IOp f -> IValue f -> Double -> ResultSet ICEES a -> IO [IFeature]
associationsToAllFeatures = undefined

-- example queries
qu1 = select @ICEES @Patient (ICond @AgeStudyStart Gt A0_2 `IAnd` ICond @Theophylline Eq True)

qu2 = let x = True in 
        select @ICEES @Patient (ICond @AgeStudyStart Gt A0_2 `IAnd` ICond @Theophylline Eq x)

qu3 = do
    cohort <- select @ICEES @Patient (ICond @AgeStudyStart Gt A0_2 `IAnd` ICond @Theophylline Eq True)
    associationsToAllFeatures @AvgDailyPM25Exposure Ge Q1 0.1 cohort

