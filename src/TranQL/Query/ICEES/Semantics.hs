{-# LANGUAGE RankNTypes, DataKinds, KindSignatures, PolyKinds, TypeFamilies, ScopedTypeVariables, TypeApplications, GADTs, FlexibleInstances, TemplateHaskell, DeriveGeneric, OverloadedStrings, AllowAmbiguousTypes, FlexibleContexts, PartialTypeSignatures, UndecidableInstances, InstanceSigs #-}
module TranQL.Query.ICEES.Semantics where

import TranQL.Semantics
import Network.HTTP.Simple hiding (Query, Proxy)
import Network.HTTP.Client.TLS
import Network.Connection
import Data.Aeson (Value, object, FromJSON(..), ToJSON(..), (.=))
import Data.Aeson.Lens (_String, key)
import Control.Lens ((^?))
import Data.Map.Strict
import Data.Singletons
import Data.Singletons.TH
import GHC.Generics (Generic)
import Data.Proxy
import Data.Text (Text, unpack)
import Data.Maybe (fromJust)

-- example ICEES query subsystem
data ISelector = Patient | Visit | AssociationsToAllFeaturesPatient | AssociationsToAllFeaturesVisit

data PatientFeature = AgeStudyStart | AvgDailyPM25Exposure | Theophylline deriving (Generic)

data VisitFeature = AgeVisit | Avg24hPM25Exposure | TheophyllineVisit deriving (Generic)
    
type family IFeature (s :: ISelector) where
    IFeature Patient = PatientFeature
    IFeature Visit = VisitFeature
    IFeature AssociationsToAllFeaturesPatient = PatientFeature
    IFeature AssociationsToAllFeaturesVisit = VisitFeature

data ICEESK = ICEES

genSingletons [''ISelector, ''PatientFeature, ''VisitFeature, ''ICEESK]

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
    ICond :: forall (f :: fs). Sing f -> Op -> IValue fs f -> CohortProp fs

data YearCohortProp (fs :: *) = YearCohortProp {
    getYear :: Int,
    getCohortProp :: CohortProp fs
    }

data ATAFProp (fs :: *) where
    ATAFCond :: forall (f :: fs). Int -- ^ year
                -> Sing f -> Op -> IValue fs f 
                -> String -- ^ cohort_id
                -> Double -- ^ maximum_p_value
                -> ATAFProp fs
    
getCohortID :: forall (fs :: *). ATAFProp fs -> String
getCohortID (ATAFCond _ _ _ _ cohortID _) = cohortID

getMaximumPValue :: forall (fs :: *). ATAFProp fs -> Double
getMaximumPValue (ATAFCond _ _ _ _ _ maximumPValue) = maximumPValue

getATAFYear :: forall (fs :: *). ATAFProp fs -> Int
getATAFYear (ATAFCond year _ _ _ _ _) = year

type family IProp (s :: ISelector) where
    IProp Patient = YearCohortProp PatientFeature
    IProp Visit = YearCohortProp VisitFeature
    IProp AssociationsToAllFeaturesPatient = ATAFProp PatientFeature
    IProp AssociationsToAllFeaturesVisit = ATAFProp VisitFeature

type CohortResultSet = String

data ATAFResultSet (s :: ISelector) = ATAFResultSet [IFeature s] deriving (Generic)

type family IResultSet (s :: Selector ICEES) where -- for some weird reason, this doesn't work with ISelector with UndecidableInstances
    IResultSet Patient = CohortResultSet
    IResultSet Visit = CohortResultSet
    IResultSet AssociationsToAllFeaturesPatient = Value -- ATAFResultSet AssociationToAllFeaturesPatient
    IResultSet AssociationsToAllFeaturesVisit = Value -- ATAFResultSet AssociationToAllFeaturesVisit
    
instance PreQuery ICEES where
    type Selector ICEES = ISelector

instance Query ICEES where
    type Prop ICEES s = IProp s -- this will not type check
    type ResultSet ICEES s = IResultSet s
    exec :: forall (s :: ISelector). Sing s -> Prop ICEES s -> IO (ResultSet ICEES s)
    exec = iexec
    
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

class ToText a where
    toText :: a -> Text

instance ToText PatientFeature where
    toText AgeStudyStart = "AgeStudyStart"
    toText AvgDailyPM25Exposure = "AvgDailyPM2.5Exposure"
    toText Theophylline = "Theophylline"

instance ToText VisitFeature where
    toText AgeVisit = "AgeVisit"
    toText Avg24hPM25Exposure = "Avg24hPM2.5Exposure"
    toText TheophyllineVisit = "TheophyllineVisit"

valueToJSON :: forall (s :: ISelector) (f :: IFeature s). Sing s ->  Sing f -> IValue (IFeature s) f -> Value
valueToJSON sings singf val =
    case sings of
        SPatient -> case singf of
            SAgeStudyStart -> toJSON val
            SAvgDailyPM25Exposure -> toJSON val
            STheophylline -> toJSON val
        SVisit -> case singf of
            SAgeVisit -> toJSON val
            SAvg24hPM25Exposure -> toJSON val
            STheophyllineVisit -> toJSON val
        SAssociationsToAllFeaturesPatient -> case singf of
            SAgeStudyStart -> toJSON val
            SAvgDailyPM25Exposure -> toJSON val
            STheophylline -> toJSON val
        SAssociationsToAllFeaturesVisit -> case singf of
            SAgeVisit -> toJSON val
            SAvg24hPM25Exposure -> toJSON val
            STheophyllineVisit -> toJSON val
                
yearCohortPropToJSON :: forall (s :: ISelector). (SingKind (IFeature s), ToText (Demote (IFeature s))) => Sing s -> YearCohortProp (IFeature s) -> Map Text Value
yearCohortPropToJSON sings p = 
    case p of
        YearCohortProp _ cohortProp -> cohortPropToJSON sings cohortProp 
            
cohortPropToJSON :: forall (s :: ISelector). (SingKind (IFeature s), ToText (Demote (IFeature s))) => Sing s -> CohortProp (IFeature s) -> Map Text Value
cohortPropToJSON sings p = 
    case p of
        ITrue -> mempty
        ICond singf op val ->
            fromList [(toText (fromSing singf), object ["operator" .= op, "value" .= valueToJSON sings singf val])]
        IAnd a b ->
            cohortPropToJSON sings a <> cohortPropToJSON sings b

atafPropToJSON :: forall (s :: ISelector). (SingKind (IFeature s), ToText (Demote (IFeature s))) => Sing s -> ATAFProp (IFeature s) -> Map Text Value
atafPropToJSON sings p = 
    case p of
        ATAFCond _ singf op val _ maximumPValue ->
            fromList [("feature", object [
                toText (fromSing singf) .= object [
                    "operator" .= op,
                    "value" .= valueToJSON sings singf val
                    ]]), ("maximum_p_value", toJSON maximumPValue)]                

propToJSON :: forall (s :: ISelector). Sing s -> IProp s -> Value
propToJSON sings p =
    case sings of
        SPatient -> toJSON (yearCohortPropToJSON sings p)
        SVisit -> toJSON (yearCohortPropToJSON sings p)
        SAssociationsToAllFeaturesPatient -> toJSON (atafPropToJSON sings p)
        SAssociationsToAllFeaturesVisit -> toJSON (atafPropToJSON sings p) where
            
instance FromJSON PatientFeature
instance FromJSON VisitFeature
instance FromJSON (IFeature s) => FromJSON (ATAFResultSet s)
            
gJSON :: Request -> Value -> IO (Response Value)
gJSON req0 reqbody = do
    manager <- newTlsManagerWith (mkManagerSettings (TLSSettingsSimple True False False) Nothing)
    let req = setRequestManager manager (setRequestBodyJSON reqbody (setRequestHeaders [("Accept", "application/JSON")] req0))
    httpJSON req

defineCohort :: forall (s :: ISelector). (YearCohortProp (IFeature s) ~ IProp s) => Sing s -> YearCohortProp (IFeature s) -> IO String
defineCohort sings p = do
    let year = getYear p
        table = case sings of
                    SPatient -> "patient"
                    SVisit -> "visit"
                    -- _ -> fail "defineCohort: unsupported selector"
    let reqbody = propToJSON sings p
    req0 <- parseRequest ("POST https://icees.renci.org/2.0.0/" ++ table ++ "/" ++ show year ++ "/cohort")
    resp <- gJSON req0 reqbody
    return (unpack (fromJust (getResponseBody @Value resp ^? key "return value". key "cohort_id" . _String)))

associationsToAllFeatures :: forall (s :: ISelector). (ATAFProp (IFeature s) ~ IProp s) => Sing s -> ATAFProp (IFeature s) -> IO Value
associationsToAllFeatures sings p = do
    let year = getATAFYear p
        cohortID = getCohortID p
        maximumPValue = getMaximumPValue p
        table = case sings of
                    SAssociationsToAllFeaturesPatient -> "patient"
                    SAssociationsToAllFeaturesVisit -> "visit"
                    -- _ -> fail "defineCohort: unsupported selector"
    let reqbody = propToJSON sings p
    req0 <- parseRequest ("POST https://icees.renci.org/2.0.0/" ++ table ++ "/" ++ show year ++ "/cohort/" ++ cohortID ++ "/associations_to_all_features")
    resp <- gJSON req0 reqbody
    return (fromJust (getResponseBody @Value resp ^? key "return value"))
        
iexec :: forall (s :: ISelector). Sing s -> Prop ICEES s -> IO (ResultSet ICEES s)
iexec sings p = case sings of
    SPatient -> defineCohort sings p
    SVisit -> defineCohort sings p
    SAssociationsToAllFeaturesPatient -> associationsToAllFeatures sings p
    SAssociationsToAllFeaturesVisit -> associationsToAllFeatures sings p


-- -- example queries
qu1 = select SICEES SPatient (YearCohortProp 2010 (ICond SAgeStudyStart Gt A0_2 `IAnd` ICond STheophylline Eq 1))

qu2 = let x = 1 in 
          select SICEES SPatient (YearCohortProp 2010 (ICond SAgeStudyStart Gt A0_2 `IAnd` ICond STheophylline Eq x))

qu3 = do
    cohort <- select SICEES SPatient (YearCohortProp 2010 (ICond SAgeStudyStart Gt A0_2 `IAnd` ICond STheophylline Eq 1))
    select SICEES SAssociationsToAllFeaturesPatient (ATAFCond 2010 SAvgDailyPM25Exposure Ge 1 cohort 0.1)

