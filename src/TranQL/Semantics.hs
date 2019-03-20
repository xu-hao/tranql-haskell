{-# LANGUAGE RankNTypes, DataKinds, KindSignatures, PolyKinds, TypeFamilies, ScopedTypeVariables, ConstraintKinds, AllowAmbiguousTypes, TypeApplications #-}
module TranQL.Semantics where

import Data.Constraint

-- all query subsystems implements this class
class PreQuery q where 
    type Selector q

class PreQuery q => Query q where
    type Prop q (s :: Selector q) :: *
    type ResultSet q (s :: Selector q) :: *
    exec :: forall (s :: Selector q). Prop q s -> IO (ResultSet q s)

class Query q => LQuery q where
    type Record q :: Selector q -> *
    toList :: forall (s :: Selector q). ResultSet q s -> [Record q s]

select :: forall (q :: *) (s :: Selector q). Query q => Prop q s -> IO (ResultSet q s)
select = exec @q @s

