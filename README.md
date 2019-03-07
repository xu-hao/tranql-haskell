# A Haskell Implementation of TranQL

## Syntax 
```
<Expr> ::= ( <Expr> )
       |   <Var>
       |   <Expr> <Expr>
       |   ASSUME <Var> : <Type> IN <Expr>
       |   LET <Var> = <Expr> IN <Expr>
       |   FRESH <Var> : <Type> IN <Expr>
       |   RETURN <Expr>
       |   SELECT <Selector>, ..., <Selector> WHERE <Expr>
       |   FROM <Expr> <Var> IN <Expr>
       |   <Expr> . <Field>
       	
<Selector> ::= <Expr> AS <Field>
	   
```

## Static Semantics

Type
```
<Type> ::= ( <Type> )
       |   <Base>
       |   Prop
       |   { <Field> : <Type>, ..., <Field> : <Type> }
       |   Set <Type>
       |   Rel <Type>
       |   <Type> -> <Type>
```

Typing Environment
```
<TEnv> ::= <eps>
       |   <TEnv>, <Var> : <Type>
```

Judgement
```
<Judgement> ::= <TEnv> |- <Expr> : <Type>
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

G, v : s |- e2 : t
----------------------
G |- ASSUME v : s IN e2 : t

G |- e1 : s    G, v : s |- e2 : t
---------------------------------
G |- LET v = e1 IN e2 : t

G, v : Rel s |- e2 : t
--------------------------
G |- FRESH v : s IN e2 : t

G |- e : t
---------------------
G |- RETURN e : Rel t

G |- e_1 : t_1    ...    G |- e_n : t_n    G |- e : Prop
-----------------------------------------------------------------------------------
G |- SELECT e_1 AS f_1, ..., e_n AS f_n WHERE e : Set { f_1 : t_1, ..., f_n : t_n }

G |- e1 : Set s    G, v : s |- e2 : t
-------------------------------------
G |- FROM e1 v IN e2 : t

G |- e : { f_1 : t_1, ..., f_n : t_n }    1 <= i <= n
-----------------------------------------------------
G |- e.f_i : t_i
```
