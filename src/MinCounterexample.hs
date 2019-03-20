{-# LANGUAGE RankNTypes, DataKinds, KindSignatures, PolyKinds, TypeFamilies, UndecidableInstances #-}
module MinCounterexample where

data I

data IC

class A q where 
    type S q

class B q where
    type P (s :: S q) :: I

instance A IC where
    type S IC = I

instance B IC where
    type P s = s -- this will not type check
   
    
