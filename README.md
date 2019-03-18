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
       |   SELECT <Expr> WHERE <Expr>
       |   LET <Var> FROM <Expr> IN <Expr>
       |   { <Selector>, ..., <Selector> }
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
G |- t : query    G |- e1 : qselector t    G |- e2 : qprop t
------------------------------------------------------------
G |- SELECT e1 FROM t WHERE e2 : qset t

```

