---
layout: post
title: "Some Thoughts on Generic² Programming in Julia"
description: ""
category: julia
tags: [julia, generic, type]
---
{% include JB/setup %}

## Generic Programming

Generic programming -- writing functions that work for multiple
different argument types -- is straightforward in Julia. To recap,
let's assume we want to write a function that sums all elements of a
vector. We could naively write

```Julia
function vintsum(xs::Vector{Int})
    s = 0
    for x in xs
        s += x
    end
    s
end
```

Of course, we might also want a function that works for floating-point
vectors, so we write

```Julia
function vfloatsum(xs::Vector{Float64})
    s = 0.0
    for x in xs
        s += x
    end
    s
end
```

This gets boring quickly -- the functions are almost identical except
for their argument types. Thus the preferred way to implement this
function in Julia is

```Julia
function vsum{T<:Number}(xs::Vector{T})
    s = zero(T)
    for x in xs
        s += x
    end
    s
end
```

This function accepts a vector of any numeric type. We restrict the
argument to numeric vectors so that addition is defined for the type
`T`. Note that we also use the function `zero(T)` to initialise the
sum `s`; this is equivalent to writing `T(0)`, but is more general,
and is preferred in Julia.

The above is standard in Julia, and could have be taken straight from
a Julia tutorial. Nothing new here.

## Generic Containers

Let's increase the level of genericity. The function `vsum` above
works for any numeric vector -- but what if the input is not a vector,
but a set? So here we go:

```Julia
function setsum{T<:Number}(xs::Set{T})
    s = zero(T)
    for x in xs
        s += x
    end
    s
end
```

Again, the function looks identical except for the type of its
argument. In addition to vectors and sets, there are many other
interesting collections -- Julia itself features a host of array
types, and the `DataStructures` package offers many additional
container types. We want to generalize the container type as well.

Our first attempt might be

```Julia
function csum0{C<:TypeConstructor, T<:Number}(xs::C{T})
    s = zero(T)
    for x in xs
        s += x
    end
    s
end
```

Here, a `TypeConstructor` is something like `Vector`, which takes
another type as argument (e.g. `Int`) to produce a type, as in
`Vector{Int}`. `Vector` by itself is not and cannot be the type of any
object in Julia; objects can only have concrete types such as
`Vector{Int}`, `Vector{Float64}`, or `Vector{Any}`. Since `Vector` is
used to construct a type, we call it here a *type constructor*.
(`Vector` is in Julia also an abstract type that is a supertype of all
types `Vector{X}`; this is different, and is not relevant in this
context.)

However, Our `csum0` function above doesn't work because Julia's
pattern matching algorithm for function dispatch does not handle type
constructor expressions such as `C{T}`. We need to write things
differently:

```Julia
function csum{CT}(xs::CT)
    T = eltype(CT)
    T<:Number || error("Argument element type $T is not a subtype of Number")
    s = zero(T)
    for x in xs
        s += x
    end
    s
end
```

Instead of matching a type constructor `C` and an element type `T`, we
need to match a single type `CT`. This means we lose the ability to
restrict `T` to numeric types, and we need to check this condition
explicitly in the body of the function.

We used the function `eltype` to obtain the element type `T` from
`CT`. As written above, the function `csum` looks like code that is
commonly found in Julia.

You might notice that we were able to obtain the element `T` from the
argument type `CT`, but not the type constructor `C`. In fact, there
is no standard way in Julia to obtain `C` -- but there is a way to get
(almost) the same effect (see below).

First, let's look at an example where we actually need to know the
type constructor `C`. Here we invert each element of a vector:

```Julia
function vinv{T<:Number}(xs::Vector{T})
    R = typeof(inv(one(T)))
    rs = Vector{R}(length(xs))
    for i in eachindex(rs)
        rs[i] = inv(xs[i])
    end
    rs
end
```

`inv(x)` calculates the reciprocal value `1/x`. For integer `x`, the
inverse is a floating-point value, i.e. it has different type. We thus
cannot just use `Vector{T}` to hold the result -- we need to calculate
the type `R` of the result. We do so by examining a sample
calculation: `one(T)` yields the value `1` of type `T`, `inv`
calculates its reciprocal, and the type of the result is the type `R`
we need to use as result type. Such type calculations are also quite
common in Julia.

On a side node, such type calculations are also commonplace in
templated C++ code, but they usually look more complex there:

```C++
typedef std::decay_t<decltype(inv(declval<T>()))> R;
```

This is unfortunate because it likely drives people away from generic
programming in C++. All the more power to a fresh approach!

We now generalize `vinv` to work with an arbitrary container type `C`.
Since, as mentioned above, Julia does not provide a way to obtain a
type constructor `C` from the argument type `CT`, we need to use a
work-around:

```Julia
function cinv{CT}(xs::CT)
    T = eltype(CT)
    T<:Number || error("Argument element type $T is not a subtype of Number")
    R = typeof(inv(one(T)))
    rs = similar(xs, R)
    for i in eachindex(rs)
        rs[i] = inv(xs[i])
    end
    rs
end
```

The function `similar` creates a new container from an existing
container (here `xs`) and a new type (here `R`). It also uses the
length of `xs` to allocate an equal-sized new container. In a way,
`similar` implements a function that takes a type `C{T}` and a type
`R`, and returns a new type `C{R}`, except that it expects a
*container object* instead of a *container type* as argument, and that
it also returns a new container object instead of just its type.

## Higher-Order Genericity

So far, we used generic objects as arguments and return values. Let's
go one step further and pass generic *types* as arguments. As example
we'll be using a "generator" function, i.e. a function that generates
a container as return value from a non-container input. Specifically,
let us write a function that takes a container type constructor and a
single element as input, and returns a container with exactly this
element.

For example, a function that creates a single-element vector would be

```Julia
function vunit{T}(x::T)
    rs = Vector{T}()
    push!(rs, x)
    rs
end
```

A function that works for many containers would be

```Julia
function cunit1{C,T}(::Type{C}, x::T)
    rs = C{T}()
    push!(rs, x)
    rs
end
```

Note that `C` is not the container type (such as `Vector{Int}`), but
only the type constructor (e.g. `Vector`), so that we need to use the
expression `C{T}` as container type.

It turns out that Julia does not have a uniform internal
representation for type constructors (such as `Vector` or `Set`), and
we thus need to use the generic `Type` as argument type; in Julia, all
type constructors are subtypes of `Type`.

We can call `cunit1` e.g. as `cunit1(Vector, 1)` or `cunit1(Set, 2)`,
and the result is as expected.

Unfortunately, this function `cunit1` is not very useful. While it is
straightforward to call `cunit1` with a manually-provided type
constructor, there is no way to obtain a type constructor from a
container type (or a container object): As mentioned above, there is
no function in standard Julia that takes the type `Vector{Int}` as
input, and returns the type constructor `Vector`. We need to look for
an alternative definition of `cunit1`. (Of course, such a function
could be added to Julia, and I argue that it should.)

In C++, the template pattern matching mechanism is more powerful than
in Julia, since one can define template specializations. This makes it
possible to determine a type constructor from a type. C++ calls these
type constructors "type templates", and if you use a type template in
a type template, you are using "template template parameters" (sic!),
a confusing terminology.

It turns out that template template parameters in C++ have a serious
disadvantage: the compiler often cannot determine whether two template
template parameters are identical, and then needs to generate multiple
identical template instantiations. This is highly undesirable. Of
course, one can use in C++ the same work-around as in Julia, which I
will describe now.

Our work-around is simply to use a type `C{X}` instead of a type
constructor `C`, using an arbitrary type `X` that we will then ignore.
In many cases, it will be convenient to use the types `Void` or `Any`
for this. (The type `Union{}` also has its charm, as it by definition
prevents non-empty containers.)

Our actual implementation of `cunit` looks like this:

```Julia
function cunit{C, T}(::Type{C}, x::T)
    rs = similar(C(), T)
    push!(rs, x)
    rs
end
```

We can call `cunit` e.g. as `cunit(Vector{Void}, 1)` or
`cunit(Set{Void}, 2)`, with result types `Vector{Int}` and `Set{Int}`,
respectively. Due to two quirks of Julia -- every type constructor is
also an abstract type, and containers usually define fall-back
constructors for abstract types as well -- the calls `cunit(Vector,
1)` and `cunit(Set, 2)` are also legal, and are interpreted as
`cunit(Vector{Any}, 1)` and `cunit(Set{Any}, 2)`, respectively, with
the same results as above.

Note that we needed to call `C()` in the call to `similar`,
constructing a throw-away empty container of type `C`. This is
unfortunate, and could be avoided by extending `similar` to accept
types as arguments as well, and not just objects.

## Remarks

So why would this higher-order genericity be interesting in practice?
As mentioned above, "regular" generic programming (as used in the
functions `csum` or `cinv`) has obvious advantages, and is commonplace
in Julia. But where would a function such as `cunit` be useful, apart
from type-theoretical discussions on monads, and blog entries
comparing Julia to C++?

In my own work, I came across this issue while experimenting with how
vector spaces would best be represented in Julia. In large numerical
simulations of e.g. coalescing binary black holes, linear algebra and
vectors spaces are everpresent, yet the algebraic concept of a "vector
space" is usually not defined anywhere in the source code. Instead,
various objects are defined, combined, added, and scaled in an ad-hoc
manner that often leads to quite tedious code.

Many types represent vector spaces -- arrays obviously do, and so do
certain tuples, most containers, and also many user-defined types. A
"vector space" should thus be represented as *trait* (or *concept* in
C++), i.e. as a property that a type might have. Each type that is a
vector space will need to implement certain functionality: create a
null vector, scale a vector, add two vectors, etc.

In addition to these operations that act on vectors, there exist also
operations on vector spaces: Given two vector spaces `V` and `U`, one
can create their sum space, product space, a power space `V^n` for a
non-negative integer `n`, and even a fiber vector space `V(U)`. Those
are vector spaces again, and all these definitions are commonly used
in numerical simulations. (Adaptive mesh refinement introduces sum
spaces where either a coarse or a fine grid is used in a region,
multi-block discretizations are product spaces, power spaces define
grids and meshes, and fiber spaces express grids and meshes holding
vectors at each element.)

Given this, I was looking for generic implementations to create sum,
product, power, and fiber spaces. That is, I wanted to implement e.g.
a type

```Julia
immutable VProduct0{V1, V2, S}
    v1::V1{S}
    v2::V2{S}
end
```

to representd a product space of `V1` and `V2` (essentially a tuple).
`V1` and `V2` represent vector spaces; they should be type
constructors that can be applied to either a scalar type, or to a
another vector space (!): `Vector{Int}` is a vector space,
`Vector{Vector{Int}}` is one as well. We thus want to write

```Julia
typealias TwoVectors{T} VProduct{Vector, Vector, T}
```

and then use e.g. `TwoVectors{Float64}` to hold some data.

Unfortunately, this type definition is not legal in Julia. In a
generic type definition such as this, `V1` and `V2` can be datatypes,
but they cannot be type constructors. One thus needs an alternative
way to represent type constructors, and then to define functions
`vnull`, `vadd`, `vscale`, etc. that implement the vector space
operations for `VProduct`.

To give a maybe more concrete motivation from linear algebra:
Block-structured matrices appear naturally in many algorithms. If one
needs a block-structured matrix where each block is dense, and the
blocks are sparsely distributed (many blocks are zero), then one
should be able to write `TwoLevelMatrix{SparseMatrix, DenseMatrix}`. 
Implementing `TwoLevelMatrix` then leads to the problems described
here, since both `SparseMatrix` and `DenseMatrix` are type
constructors, not types.

## Future

Generic programming is ubiquitous in Julia. Higher-order generic
programming, i.e. using generic type constructors instead of just
generic types, seems alluring, and is able to provide elegant
solutions to otherwise tedious coding exercises, at least in other
languages. It remains to be seen in how far this is currently possible
in Julia. I think most of the bits and pieces are already there,
although certain things are still more cumbersome than they should be.

Julia's competitor in this respect is not C++. While C++ templates are
more powerful than Julia's generic types, and thus better suited in
theory, its strange and convoluted template syntax and type semantics
make it quite difficult to use in practice. Haskell, of course,
supported higher-order generic programming already a decade ago, but
is coming from a very different angle. In Haskell, one needs to
understand and implement every corner case correctly before the code
compiles, and before one can begin to experiment. In Julia, where
types are objects that can be manipulated at run time, experimentation
is much easier, but one foregoes the safety of a much stronger typing
harness. I conjecture that Julia's approach makes it easier than
Haskell's to work in large, interdisciplinary open-source
collaborations.
