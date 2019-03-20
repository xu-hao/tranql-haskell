{-# LANGUAGE RankNTypes, DataKinds, KindSignatures, PolyKinds, TypeFamilies, ScopedTypeVariables, TypeApplications, GADTs, FlexibleInstances, TemplateHaskell, DeriveGeneric, OverloadedStrings, AllowAmbiguousTypes, FlexibleContexts, PartialTypeSignatures #-}
module TranQL.Query.ICEES.Semantics where

import TranQL.Semantics
import Network.HTTP.Simple hiding (Query, Proxy)
import Data.Aeson
import Data.Map.Strict
import Data.Singletons
import Data.Singletons.TH
import GHC.Generics (Generic)
import Data.Proxy

-- example ICEES query subsystem
data ISelector = Patient | Visit | AssociationToAllFeaturesPatient | AssociationToAllFeaturesVisit

data PatientFeature = AgeStudyStart | AvgDailyPM25Exposure | Theophylline

data VisitFeature = AgeVisit | Avg24hPM25Exposure | TheophyllineVisit
    
type family IFeature (s :: ISelector) where
    IFeature Patient = PatientFeature
    IFeature Visit = VisitFeature
    IFeature AssociationToAllFeaturesPatient = PatientFeature
    IFeature AssociationToAllFeaturesVisit = VisitFeature

genSingletons [''ISelector, ''PatientFeature, ''VisitFeature]

data Op = Le | Ge | Lt | Gt | Eq | Ne

data AgeBins = A0_2
    | A3_17
    | A18_34
    | A35_50
    | A51_69
    | A70_89

type family IValue (fs :: *) (f :: fs) :: * where
    IValue PatientFeature AgeStudyStart = AgeBins
    IValue PatientFeature AvgDailyPM25Exposure = Int -- currently it is cumbersome to express type refinement in Haskell {i : Int | 1 <= i <= 5}, so we use Int
    IValue PatientFeature Theophylline = Int
    IValue VisitFeature AgeVisit = AgeBins
    IValue VisitFeature Avg24hPM25Exposure = Int -- currently it is cumbersome to express type refinement in Haskell {i : Int | 1 <= i <= 5}, so we use Int
    IValue VisitFeature TheophyllineVisit = Int

data CohortProp (fs :: *) where
    ITrue :: CohortProp fs
    IAnd :: CohortProp fs -> CohortProp fs -> CohortProp fs
    ICond :: forall (f :: fs). SingI f => Proxy f -> Op -> IValue fs f -> CohortProp fs

data ATAFProp (fs :: *) where
    ATAFCond :: forall (f :: fs). SingI f => Proxy f -> Op -> IValue fs f 
                -> String -- ^ cohort_id
                -> Double -- ^ maximum_p_value
                -> ATAFProp fs
    
type family IProp (s :: ISelector) where
    IProp Patient = CohortProp PatientFeature
    IProp Visit = CohortProp VisitFeature
    IProp AssociationToAllFeaturesPatient = ATAFProp PatientFeature
    IProp AssociationToAllFeaturesVisit = ATAFProp VisitFeature

data CohortResultSet = CohortResultSet String deriving (Generic)

data ATAFResultSet (s :: ISelector) = ATAFResultSet [IFeature s]

type family IResultSet (s :: Selector ICEES) where
    IResultSet Patient = CohortResultSet
    IResultSet Visit = CohortResultSet
    IResultSet AssociationToAllFeaturesPatient = ATAFResultSet AssociationToAllFeaturesPatient
    IResultSet AssociationToAllFeaturesVisit = ATAFResultSet AssociationToAllFeaturesVisit
    
data ICEES = ICEES

instance PreQuery ICEES where
    type Selector ICEES = ISelector

instance Query ICEES where
    type Prop ICEES s = IProp s
    type ResultSet ICEES s = IResultSet s
    -- exec = iexec
    
instance ToJSON Op where
    toJSON = toJSON . opToJSON where
        opToJSON :: Op -> String
        opToJSON Le = "<="
        opToJSON Ge = ">="
        opToJSON Lt = "<"
        opToJSON Gt = ">"
        opToJSON Eq = "="
        opToJSON Ne = "<>"

instance ToJSON AgeBins where
    toJSON = toJSON . ageBinsToJSON where
        ageBinsToJSON :: AgeBins -> String
        ageBinsToJSON A0_2 = "0-2"
        ageBinsToJSON A3_17 = "3-17"
        ageBinsToJSON A18_34 = "18-34"
        ageBinsToJSON A35_50 = "35-50"
        ageBinsToJSON A51_69 = "51-69"
        ageBinsToJSON A70_89 = "70-89"

class ToString a where
    toString :: a -> String

instance ToString PatientFeature where
    toString AgeStudyStart = "AgeStudyStart"
    toString AvgDailyPM25Exposure = "AvgDailyPM2.5Exposure"
    toString Theophylline = "Theophylline"

instance ToString VisitFeature where
    toString AgeVisit = "AgeVisit"
    toString Avg24hPM25Exposure = "Avg24hPM2.5Exposure"
    toString TheophyllineVisit = "TheophyllineVisit"

valueToJSON :: forall (s :: ISelector) (f :: IFeature s). (SingI s, SingI f) => IValue (IFeature s) f -> Value
valueToJSON val =
    case sing :: (SingI s) => Sing s of
        SPatient -> case sing :: (SingI f) => Sing f of
            SAgeStudyStart -> toJSON val
            SAvgDailyPM25Exposure -> toJSON val
            STheophylline -> toJSON val
        SVisit -> case sing :: (SingI f) => Sing f of
            SAgeVisit -> toJSON val
            SAvg24hPM25Exposure -> toJSON val
            STheophyllineVisit -> toJSON val
        SAssociationToAllFeaturesPatient -> case sing :: (SingI f) => Sing f of
            SAgeStudyStart -> toJSON val
            SAvgDailyPM25Exposure -> toJSON val
            STheophylline -> toJSON val
        SAssociationToAllFeaturesVisit -> case sing :: (SingI f) => Sing f of
            SAgeVisit -> toJSON val
            SAvg24hPM25Exposure -> toJSON val
            STheophyllineVisit -> toJSON val
                

cohortPropToJSON :: forall (s :: ISelector). (SingI s, SingKind (IFeature s), ToString (Demote (IFeature s))) => CohortProp (IFeature s) -> Map String Value
cohortPropToJSON p = 
    case p of
        ITrue -> mempty
        ICond (Proxy :: Proxy f) op val ->
            fromList [(toString (fromSing (sing :: Sing f)), toJSON (fromList [("operator" :: String, toJSON op),("value" :: String, valueToJSON @s @f val)]))]
        IAnd a b ->
            cohortPropToJSON @s a <> cohortPropToJSON @s b

atafPropToJSON :: forall (s :: ISelector). (SingI s, SingKind (IFeature s), ToString (Demote (IFeature s))) => ATAFProp (IFeature s) -> Map String Value
atafPropToJSON p = 
    case p of
        ATAFCond (Proxy :: Proxy f) op val _ maximumPValue ->
            fromList [("feature", toJSON (fromList [(toString (fromSing (sing :: Sing f)), toJSON (fromList [("operator" :: String, toJSON op),("value" :: String, valueToJSON @s @f val)]))])), ("maximum_p_value", toJSON maximumPValue)]                

propToJSON :: forall (s :: ISelector). SingI s => IProp s -> Value
propToJSON p =
    case sing :: (SingI s) => Sing s of
        SPatient -> toJSON (cohortPropToJSON @s p)
        SVisit -> toJSON (cohortPropToJSON @s p)
        SAssociationToAllFeaturesPatient -> toJSON (atafPropToJSON @s p)
        SAssociationToAllFeaturesVisit -> toJSON (atafPropToJSON @s p) where
            

defineCohort :: forall (s :: ISelector). (SingI s, CohortProp (IFeature s) ~ IProp s, FromJSON (IResultSet s)) => CohortProp (IFeature s) -> IO (ResultSet ICEES s)
defineCohort p = do
    let year = "2010"
    let table = case sing :: SingI s => Sing s of
                    SPatient -> "patient"
                    SVisit -> "visit"
                    -- _ -> fail "defineCohort: unsupported selector"
    let reqbody = propToJSON @s p
    req0 <- parseRequest ("POST https://icees.renci.org/2.0.0/" ++ table ++ "/" ++ year ++ "/cohort")
    let req = setRequestBodyJSON reqbody (setRequestHeaders [("Content-Type", "application/JSON"), ("Accept", "application/JSON")] req0)
    resp <- httpJSON req
    return (getResponseBody resp)

    
-- instance FromJSON (ResultSet ICEES a)



-- -- example queries
qu1 = select @ICEES @Patient (ICond (Proxy @AgeStudyStart) Gt A0_2 `IAnd` ICond (Proxy @Theophylline) Eq 1)

-- qu2 = let x = True in 
--         select @ICEES @Patient (ICond @AgeStudyStart Gt A0_2 `IAnd` ICond @Theophylline Eq x)

-- qu3 = do
--     cohort <- select @ICEES @Patient (ICond @AgeStudyStart Gt A0_2 `IAnd` ICond @Theophylline Eq True)
--     associationsToAllFeatures @AvgDailyPM25Exposure Ge 1 0.1 cohort

