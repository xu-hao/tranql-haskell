What conditions with a known genetic cause are most representative of the class of conditions which are phenotypically similar to a given condition?

C : condition

g : genetic

D : condition

d : given condition

let P = similar(d) in
    Q(C,X) = cause(g, C) & represent(C, P, X) in
    argmax_Y Q(X, Y)
      
SELECT g->(cause)->C
WHERE C = represent(P) 
AND P(D) = D->(similar)->d
      
datafun

to_set :: Cohort a -> Set a

to_prop	:: CohortProp a	-> Prop	a

return :: a -> rel a

join ::	rel (rel a) -> rel a

p :: rel a -> rel a

microkanren

rel a

acl

milawa

milawa self-verifying theorem prover for an acl2-like logic

lcf approach

https://arxiv.org/pdf/1808.09701.pdf

https://github.com/gregr/experiments/blob/master/acl2/mergesort.scm
