# Next Step

* implement a visualizer to the dot format to see the computational graph
* use free monad to represent the syntax tree
* use recursion scheme makes more senses here
  * understand how recursion scheme can persist tail recursion properties
* defined the algebra of the term
* take a look at paper connecting recursion schemes and neural networks

* explicit pattern matching, no more fail_with
* support secondary greek - gamma - rate of change over delta 
* support monte carlo based pricing


* Generate Dot Graph
> dune exec -- bin/main.exe | dot -Tpdf > example/bs.pdf

# Monte carlo pricer
1. Express the discrete model in time steps
2. a random number generator 
3. run the discrete model for a certain time steps to reach expiry time
4. take average of each path - express pay off function
5. discounted to the present value

- [ ] merge tag_expr and expr together
* pricing reproducibility - pass the seed as an input
* need to introduce max operation
* need to introduce power operation to reduce the complexity computational graph

# Currnet Issue
- [ ] Duplication of tag expr
- [ ] not support for logical operation
- [ ] not support for if-then-else control flow
- [ ] Visulization including i.e sub expression

# Debug

* the bug is a mistake in the implementation of normal distribution
  * the numerical approximation only works on postive number
  * the leads to the problem
    * how to deal with if then else expression and how to differentiate it
    * differentiate the control flow (automatic?)

* api design
  * functor : 'a Expr -> ('a -> 'b) -> 'b Expr
  

* use structured note at as an example for hull white 1 factor

# Limitation
* lack of automation

# Learn

## OCaml Type System
* [Polymorphism and its limitations](https://v2.ocaml.org/manual/polymorphism.html)
  * how to rececover gnericity
    * weakly polymorphic type variable - a placeholder for a concrete type that's unknown atm
      * weakly type must be known during the compilation of a compilation unit
      * hence it can be be placed into mli file
      * value restriction
        * if soemthing rely on persistent mutable stats behind the screne, then it should be give a weak type
        * difference between function definition and value definition
      * relaxed value restriction
        * covariant - Functor f - s is a subtype t iff f s is a subtype of f t
        * contravariant 
        * scenarios
          * abstrac data types
          * variance types
    * generic polymorphic type variables
    * polymorphic recursion
      * need to introduce a different type variable 
        * for every application of the recursive function
    * **type variables may or may not be polymorphic**
    * understand type checker behaviour
    * A fundmental issue of genericity
      * the type variable are introduced at the start of definitio hence may be uniformed unneccesarily
      * univerally quantified types to rescue
        * no native support in ocaml
        * achieved using universally quantified record fields
          * [record fields](https://v2.ocaml.org/manual/typedecl.html#field-decl)
          * [polynomial type expression](https://v2.ocaml.org/manual/types.html#poly-typexpr)

* [GADT](https://v2.ocaml.org/manual/gadts-tutorial.html)
  * absence of an explicit polymorphic annotation
    * $App_'b denotes an existential type named by the compiler
    * existential type variable cannot escape its scope
* Existential vs. Universally quantified types

## Next Step

- [x] Simplifying the description true to be of
  * type bool - support logical operation
  * type float - support floating point operation
- [x] remove one/zero
- [x] add erf as an operator
- [ ] generalise visual to debug intermidate states of computational graphs
- [ ] complete support for max/min
- [ ] add if-then-else
- [ ] monte carlo pricing
- [ ] think about how recursion-scheme can come into plays
