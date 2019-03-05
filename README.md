# tranql-haskell

## Syntax 
```
<Expr> ::= ( <Expr> )
       |   <Var>
       |   <Expr> <Expr>
       |   LET <Var> = <Expr> IN <Expr>
       |   FRESH <Var> IN <Expr>
       |   SELECT <Selector>, ..., <Selector> WHERE <Expr>

<Selector> ::= <Expr> AS <Var>
	   
```

## Static Semantics

Type
```
<Type> ::= ReI
       |   Ind
       |   Prop
       |   Set
       |   <Type> -> <Type>
```

Context
```
<Ctx> ::= <eps>
      |   <Ctx>, <Var> : <Type>
```

Judgement
```
<Context> |- <Expr> : <Type>
```

Typing Rule
```
G |- e : t
------------
G |- (e) : t

-----------------
G, v : t |- v : t

G |- e1 : s -> t    G |- e2 : s
-------------------------------
G |- e1 e2 : t

G |- e1 : ReI -> t    G |- e2 : Ind
----------------------------------
G |- e1 e2 : t

G |- e1 : s    G, v : ReI |- e2 : t
----------------------------------
G |- FRESH v IN e2 : t

G |- e1 : s    G, v : s B |- e2 : t
-----------------------------------
G |- LET v = e1 IN e2 : t

G |- e_1 : Ind    ...    G |- e_n : Ind    G |- e : Prop
--------------------------------------------------------
G |- SELECT e_1 AS v_1, ..., e_n AS v_n WHERE e : Set
```
