# Design considerations and decisions

## Internal mechanics

### Storage

We avoid asd  
### Code generation

When the above nodes would allocate memory, we can omit generating any evaluation code for them. The "result"
can be accessed directly through their names. For example `add (scl 5) (var "x" double)` should generate
```cpp
m1 = 5 + x;
```
and not
```cpp
m1 = 5;
m2 = x;
m3 = m1 + m2;
```
However, when the same nodes inherit their target memory, they have to explicitly set that memory to their
value at evaluation, so we need to generate code for an assignment. For example in the identity function
`lam [var "x" double] (var "x" double)` the `var` node in the body obviously has to generate some code, so the whole construct becomes
```cpp
[=](auto x) {
    m1 = x;
}
```
# Lambda representation (Currying)
The uncurried form is more compact, which helps pattern-based transformations. Partial application can be supported either by still generating curried lambdas, or by replacing incomplete applications with reduced lambdas and let bindings, e.g. `app (lam [x,y] (add x y)) [scl 1]` --> `lam [y] (let x (scl 1) (add x y))`. The latter was chosen, because uncurried lambdas are more natural in C++, and probably more efficient as well.
