LambdaGen
==========
This tool supports automatic optimization and parallel evaluation of linear algebraic computations on GPUs. It's primary purpose is to let scientists focus on science while providing them with reasonable utilization of the available massively parallel architecture. 
# Usage
 - You build the expression tree of your calculation using high level
   primitives known from functional programming (lambda expressions,
   higher order functions, etc.).
 - We analyze and transform the expression tree to make it more suitable for GPU evaluation, and
 - determine how to allocate threads and memory to effectively utilize
   your hardware.
 - Finally we produce SYCL code, which you just need to compile&run to get the result of your calculation.
# Examples
There is a very basic but complete example in [eval](https://github.com/leanil/LambdaGen/tree/master/eval). You can also find more complex expression trees in [FunctionalTest.hs](https://github.com/leanil/LambdaGen/blob/master/src/FunctionalTest.hs), and the generated SYCL codes in [test](https://github.com/leanil/LambdaGen/tree/master/test).

# Pre-requisites
 - [ComputeCpp](https://www.codeplay.com/products/computesuite/computecpp) OR
 - [triSYCL](https://github.com/triSYCL/triSYCL)
# Expression primitives
**scl x**: Scalar constant with value x. Its type is `double`.  
**add a b**, **mul a b**: Scalar addition and multiplication. The type of a, b and the result is `double`.  
**vecView id [d<sub>1</sub>,...,d<sub>n</sub>]**: A tensor of size d<sub>1</sub> x … x d<sub>n</sub> referencing used data id.  
Its type is `power (power (… power double (dim dn) … ) (dim d2)) (dim d1)`.  
**var id t**: The variable of a lambda function with given identifier and type.  
**lam v t**: Lambda abstraction that binds variable v in the expression t. If the type of v and t is a and b, then the result is of type `arrow a b`.  
Higher order functions:
<table>
<tr><th>Operation</th><th>Parameter types</th><th>Result type</th><th>Result</th></tr>
<tr><td><b>app(f, x)</b></td><td> arrow(a,b), a</td><td>b</td><td>f(x)</td></tr>
<tr><td><b>map(f, [x<sub>1</sub>,…,x<sub>n</sub>])</b></td><td>arrow(a,b), power(a,n)</td><td>power(b,n)</td><td>[f(x<sub>1</sub>),...,f(x<sub>n</sub>)]</td></tr>
<tr><td><b>zipWith(f, [x<sub>1</sub>,…,x<sub>n</sub>],<br>[y<sub>1</sub>,…,y<sub>n</sub>])</b></td><td>arrow(a,arrow(b,c)), power(a,n), power(b,n)</td><td> power(c,n)</td><td>[f(x<sub>1</sub>,y<sub>1</sub>),...,f(x<sub>n</sub>,y<sub>n</sub>)]</td></tr>
<tr><td><b>reduce(f,[x<sub>1</sub>,…,x<sub>1</sub>])</b></td><td>arrow(a,arrow(a,a)), power(a,n)</td><td>a</td><td> x<sub>1</sub> (n = 1)<br>reduce(f,[f(x<sub>1</sub>,x<sub>2</sub>),…,x<sub>n</sub>]) (n > 1)</td></tr>
</table>
