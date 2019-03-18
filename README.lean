namespace TranQL

notation a `>>=` b := bind a b


constant IO : Type → Type
instance IO_Monad : monad IO := sorry

#check (return 1 : IO int) >>= (λ _ , return 2 : int → IO int)

-- all query subsystems implements this class
structure query := 
    (selector : Type)
    (prop : Type)
    (set : selector → Type)
    (exec : Π (s : selector), prop → IO (set s))

-- all query subsystems implements this class
structure lquery extends query := 
    (record : selector → Type)
    (toList : Π (s : selector), set s → list (record s))

-- FROM q SELECT s WHERE p
def select (q : query) (s : query.selector q) (p : query.prop q) : IO (query.set q s) :=
    query.exec q s p

notation `SELECT` a `FROM` b `WHERE` c := select b a c

notation `BIND` := bind

end TranQL

namespace ICEES
open TranQL
-- example ICEES query subsystem
inductive ISelector : Type
| patient : ISelector

inductive IFeature : Type
| AgeStudyStart : IFeature
| AvgDailyPM25Exposure : IFeature
| Theophylline : IFeature

inductive IOp : IFeature → Type
| le : Π {f : IFeature}, IOp f
| ge : Π {f : IFeature}, IOp f
| lt : Π {f : IFeature}, IOp f
| gt : Π {f : IFeature}, IOp f
| eq : Π {f : IFeature}, IOp f
| ne : Π {f : IFeature}, IOp f

inductive rational : Type
| frac : int -> int -> rational 

inductive ageBins : Type
| a0_2
| a3_17
| a18_34
| a35_50
| a51_69
| a70_89

def IValue : IFeature → Type
| IFeature.AgeStudyStart := ageBins
| IFeature.AvgDailyPM25Exposure := int
| IFeature.Theophylline := bool

inductive IProp : Type
| ITrue : IProp
| IAnd : IProp → IProp → IProp
| ICond : Π (f : IFeature), IOp f → IValue f → IProp

notation a `<=` b := IProp.ICond a IOp.le b
notation a `>=` b := IProp.ICond a IOp.ge b
notation a `<` b := IProp.ICond a IOp.lt b
notation a `>` b := IProp.ICond a IOp.gt b
notation a `=` b := IProp.ICond a IOp.eq b
notation a `<>` b := IProp.ICond a IOp.ne b
notation a `AND` b := IProp.IAnd a b
notation `TRUE` := IProp.ITrue

inductive ISet : Type
| ICohort : string → ISet

def ICEES := {query.
    selector := ISelector,
    prop := IProp,
    set := λ _, ISet,
    exec := sorry
}

constant associationsToAllFeatures : Π (f : IFeature), IOp f → IValue f → rational → ISet → list IFeature

-- example query
open ISelector
open IFeature
open ageBins

def q1 := SELECT patient FROM ICEES WHERE (AgeStudyStart > a0_2 AND Theophylline = true)

def q2 := 
    let x := true in SELECT patient FROM ICEES WHERE (AgeStudyStart > a0_2 AND Theophylline = x)

def q3 := 
    BIND (SELECT patient FROM ICEES WHERE (AgeStudyStart > a0_2 AND Theophylline = true)) (associationsToAllFeatures AvgDailyPM25Exposure ge 1)

end ICEES