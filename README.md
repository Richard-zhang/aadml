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

* pricing reproducibility - pass the seed as an input
* need to introduce max operation
* need to introduce power operation to reduce the complexity computational graph

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
* lack of automatics
