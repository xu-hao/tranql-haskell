# A Haskell Implementation of TranQL

## Syntax 
```
<Expr> ::= ( <Expr> )
       |   <Var>
       |   <Expr> <Expr>
       |   RETURN <Expr>
       |   BIND <Expr> <Expr>
       |   FROM <Var> SELECT <Expr> WHERE <Expr>
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

