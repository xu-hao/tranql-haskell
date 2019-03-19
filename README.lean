namespace rational

inductive rational : Type
| frac : int -> int -> rational 

def rational_div : rational → rational → rational
| (rational.frac a b) (rational.frac c d) := rational.frac (a * d) (b * c)

def rational_one : rational := rational.frac 1 1

def rational_add : rational → rational → rational
| (rational.frac a b) (rational.frac c d) := rational.frac (a * d + b * c) (b * d)

instance rational_has_div : has_div rational := {has_div.
    div := rational_div
}

instance rational_has_add : has_add rational := {has_add.
    add := rational_add
}

instance rational_has_one : has_one rational := {has_one.
    one := rational_one
}

end rational

namespace io

constant io : Type → Type

instance io_monad : monad io := sorry

end io

namespace TranQL

open rational
open io

-- all query subsystems implements this class
structure query := 
    (selector : Type)
    (prop : Type)
    (set : selector → Type)
    (exec : Π (s : selector), prop → io (set s))

-- all query subsystems implements this class
structure lquery extends query := 
    (record : selector → Type)
    (toList : Π (s : selector), set s → list (record s))

-- FROM q SELECT s WHERE p
def select (q : query) (s : query.selector q) (p : query.prop q) : io (query.set q s) :=
    query.exec q s p

notation `SELECT` a `FROM` b `WHERE` c := select b a c

notation `BIND` := bind

end TranQL

namespace ICEES
open TranQL
open int
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

inductive ageBins : Type
| a0_2
| a3_17
| a18_34
| a35_50
| a51_69
| a70_89

inductive quintile : Type
| q1
| q2
| q3
| q4
| q5

def IValue : IFeature → Type
| IFeature.AgeStudyStart := ageBins
| IFeature.AvgDailyPM25Exposure := quintile
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

notation `.<=` := IOp.le
notation `.>=` := IOp.ge
notation `.<` := IOp.lt
notation `.>` := IOp.gt
notation `.=` := IOp.eq
notation `.<>` := IOp.ne

inductive ISet : Type
| ICohort : string → ISet

def ICEES := {query.
    selector := ISelector,
    prop := IProp,
    set := λ _, ISet,
    exec := sorry
}

open rational
open io

constant associationsToAllFeatures : Π (f : IFeature), IOp f → IValue f → rational → ISet → io (list IFeature)

-- example query
open ISelector
open IFeature
open ageBins
open quintile

def qu1 := SELECT patient FROM ICEES WHERE (AgeStudyStart > a0_2 AND Theophylline = true)

def qu2 := 
    let x := true in SELECT patient FROM ICEES WHERE (AgeStudyStart > a0_2 AND Theophylline = x)

def qu3 := 
    (SELECT patient FROM ICEES WHERE (AgeStudyStart > a0_2 AND Theophylline = true)) 
    >>= 
    λ cohort, associationsToAllFeatures AvgDailyPM25Exposure .>= q1 (1 / 10) cohort

end ICEES