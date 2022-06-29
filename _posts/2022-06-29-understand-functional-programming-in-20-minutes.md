---
layout: post
title: Understand Functional Programming in 20 Minutes
categories: [Software, Design, Functional programming]
---

Functional programming has a reputation for being notoriously hard to understand.
In this post, we consistently examine its basic principles, abstractions,
and building blocks. Hopefully, after reading this, you will be able to decide for
yourself whether you really want to embark on a trip into this wonderland.

# Down the Rabbit Hole: Foundational Principles

Let us overview some basic principles employed in functional programming. 
They have a profound impact on how the functional programs are designed.

## Immutability and Referential Transparency

Despite a widespread belief, it is not the presence of
[lambdas](https://en.wikipedia.org/wiki/Anonymous_function), and even [lambda
calculus](https://en.wikipedia.org/wiki/Lambda_calculus) makes programming
functional. On the surface, everything is much simpler. In a functional program,
all data structures should be
[immutable](https://en.wikipedia.org/wiki/Persistent_data_structure), and
functions must be [referentially
transparent](https://en.wikipedia.org/wiki/Referential_transparency) (free of
side effects). A referentially transparent expression could always be replaced
with its value without changing the result of the program. This means that, like
mathematical functions, our functions should always return the same value when
called with the same set of arguments. But there is a little quirk. In a truly
functional program, all functions should have only one argument to be able to
participate in functional composition. We will see how to
[deal](https://en.wikipedia.org/wiki/Currying) with this below.

## First-class functions

Having [first-class
functions](https://en.wikipedia.org/wiki/First-class_function) means that 
functions could be returned as a value or passed as arguments. In such a
context, [anonymous functions](https://en.wikipedia.org/wiki/Anonymous_function)
make sense. In turn, the existence of functions that could be defined elsewhere
implies the existence of [lexical
closures](https://en.wikipedia.org/wiki/Closure_(computer_programming)). In the
functional languages that allow side effects, lexical closures may become a
basis of poor man's encapsulation. But more often, first-class functions are
employed as the mechanism for another type of abstraction: higher-order
functions.

## Recursion and Corecursion

[Recursion](https://en.wikipedia.org/wiki/Recursion_(computer_science)) is a way
of solving problems by representing it as a combination of some base case and a
smaller version of the same problem. Usually, recursion is implemented through
self-referencing function calls or through [mutual
recursion](https://en.wikipedia.org/wiki/Mutual_recursion). While recursion
starts with the last step and continues until the base case is achieved,
corecursion starts with the base case and continues until the first step.
Depth-first and breadth-first [tree
traversal](https://en.wikipedia.org/wiki/Tree_traversal) algorithms are examples
of recursion and corecursion respectively.

## Higher-order functions

[Higher-order functions](https://en.wikipedia.org/wiki/Higher-order_function)
are functions that accept other functions to use them as a part of the
algorithm. They may be considered as an abstraction device similar to the 
[template method](https://en.wikipedia.org/wiki/Template_method_pattern) in OOP. The
[map](https://en.wikipedia.org/wiki/Map_(higher-order_function)) is a simple
example. 

In the context of functional programming, combinators are the
higher-order functions that do not have dependencies on the outer context (do
not have free variables) and use only function applications to produce results.
[Y-combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed-point_combinators_in_lambda_calculus)
is a curious example, which allows achieving self-recursion in languages that do
not support referring to a function by its name from its code. In general, 
combinators are used to hierarchically organize abstractions, reducing code
complexity.

## Folding

In purely functional languages, the mutation is prohibited, and we can not
create a loop that modifies a variable through its iterations. To iterate we
can only rely on recursion. A higher-order function called
[fold](https://en.wikipedia.org/wiki/Fold_(higher-order_function)) (also known
as reduce) is the generalization of this recursive approach to iteration. It
takes a recursive data structure, such as a list, along with a combining
function, and returns a value that combines all the elements of the given data
structure. Recursion may be implemented in two different ways, by
combining the first or the last element of the data structure with the recursive
result of the combination of the rest. There are two corresponding kinds of the
fold: the right (`foldr`, for example `1 + [2 + [3 + [4 + 5]]]`) and the left
(`foldl`, for example `[[[1 + 2] + 3] + 4] + 5`). The Direction of folding may be
important if the supplied combining function is not commutative. [Tail-call
optimization](https://en.wikipedia.org/wiki/Tail_call) is required to circumvent
the issue of stack overflow on large inputs.

## Functional composition

Because functions should not produce side effects, there is little sense in
sequential function evaluation. And indeed, functional languages generally
support only one statement per function. Although, if pattern matching is
involved, this statement may contain more than one path of execution. One of the
ways to achieve sequential evaluation in a single statement is to compose
functions. For instance, while imperative programmers say `f1(); f2();`,
functional programmers say `f2(f1());`. Thus, the [function
composition](https://en.wikipedia.org/wiki/Function_composition_(computer_science))
becomes the key primitive used to make abstractions in the functional world.

Functional programmers invented many ways to compose various 
things with each other. For example, Haskell has
the following compositional operators: `. $ <$ <$> $> <> <* <*> *> >> >>= =<< >=> <=< <|>`.
So you can conveniently make an abstraction by saying `f3 = f2 . f1`
which is equivalent to 
`f3 = f2(f1())` in the [point free](https://en.wikipedia.org/wiki/Tacit_programming) notation. 
And if this is not enough, you can always create your own operators. 
What a splendid language!

## Currying and Partial Application

To be a subject for the composition, functions should accept only one argument.
[Currying](https://en.wikipedia.org/wiki/Currying) is the operation that 
creates a single-parameter function from a multi-parameter one. A curried
function is a single-argument function that simply returns another function
with the number of arguments decreased by one. On the other hand, [partial application](https://en.wikipedia.org/wiki/Partial_application)
(not to be confused with [partial functions](https://en.wikipedia.org/wiki/Partial_function))
allows binding of predefined values to some function parameters, 
which also results in a function with a lesser number of arguments.

## Lazy and Strict Evaluation

When working with recursive algorithms and data structures, it makes sense to
defer the evaluation of the recursive part until it is actually needed. Such
deferring based on syntactic constructs is called [lazy
evaluation](https://en.wikipedia.org/wiki/Lazy_evaluation) and allows, for
example, the creation of potentially infinite sequences. Such tricks are
impossible in strict languages, where all expressions are evaluated completely
just after they are encountered.

# Through the Looking-Glass: Where We Meet Category Theory 

Design of purely functional programs heavily relies on category theory 
and the underlying algebraical laws of the used functional primitives.
We will only scratch the surface here to give a glimpse of what purely functional
design looks like.

## Algebraic Data Types

Although functional languages may be dynamically typed, there are plenty of
statically-typed ones. At first glance, [functional type
systems](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) with
their [typeclasses](https://en.wikipedia.org/wiki/Type_class) may seem complex
and unwieldy. It is because they may really be complex and unwieldy.

There are two kinds of [algebraic data
types](https://en.wikipedia.org/wiki/Algebraic_data_type) (ADTs): sum types and
product types. They vary in the way by which their number of inhabiting values
is calculated. For example, the number of possible inhabiting values of
2-[tuple](https://en.wikipedia.org/wiki/Tuple) of bytes, which is a product
type, is 256x256 = 65536. It includes possible combinations from `[0, 0]` to
`[255, 255]`. On the other hand, the number of inhabitants of the Boolean, a sum
type, is 2: `true` and `false`. Product types and sum types are collectively
called "algebraic" because they have algebraic
[properties](https://stanford-cs242.github.io/f19/lectures/03-2-algebraic-data-types.html)
similar to normal integers.

In practice, the sum types are often employed as containers that wrap some
tagged values which could be processed with pattern matching, spawning their
own paths of execution. Such wrappers are called "contexts". For example, the
following listing defines the `Maybe` sum type with two value constructors used
as tags: `Just` that takes a parameter and parameterless `Nothing`.

```haskell
data Maybe a = Just a | Nothing
x = Just 10 -- wrap an Int into the Maybe container 
```

Similar to a null-reference, the `Maybe` type is used to distinguish between the
absent and fulfilled results of a computation. It is more safe than `null`,
dubbed as the billion dollar mistake, because in pattern matching compiler will
enforce the use of the `Nothing` branch.

## Pattern matching

Pattern matching offers syntactic constructs needed to destructure and recombine
values stored in ADTs. 

```haskell
add :: Int -> Int -> Int       -- Hindley-Milner signatrue declaration
add x y = x + y                -- add takes two Ints and returns an Int 

add2 :: Maybe Int -> Maybe Int -- add2 takes an Int packed into a Maybe 
add2 x =                       -- and also returns an Int packed into a Maybe
  case x of                    -- the pattern matching takes place here
    Just x' -> Just (add x' 2) -- x' is the Int destructured from the Maybe wrapper
    Nothing -> Nothing         -- Nothing is left as it is
```

Because exceptions disrupt the control flow of a program, functional languages
use various tagged values to indicate errors in computations. Being returned
somewhere in the middle of the chain of composed functions, `Nothing` will
short-circuit the computation. Such change of a computation path or a wrapper is
called an "effect" (not to be confused with side-effects).

The two following sections contain some highly abstract stuff, but it is important.
Please, do not switch the channel.

## Some Category Theory 

A category consists of a set of objects and a set of
[morphisms](https://en.wikipedia.org/wiki/Morphism) between the objects.
Morphism `f` between objects `a` and `b` is written as `f: a -> b`. Morphisms
compose. In the context of functional programming, objects are types and
morphisms are functions. Morphisms that map to the same type (`f: a -> a`) are
called endomorphisms. Morphisms may map not only between types, but also between
categories, for example:

```haskell
-- a function that takes type a and returns type b is mapped to a function
-- that takes and returns these types wraped into the Maybe container
F :: (a -> b) -> (Maybe a -> Maybe b)
```

Such higher-order morphisms are called
[functors](https://en.wikipedia.org/wiki/Lift_(mathematics)), and mapping into
some other category (context) is called
[lifting](https://en.wikipedia.org/wiki/Lift_(mathematics)). Morphisms that
preserve the algebraic structure of the objects, as in the definition above, are
called homomorphisms. There are also [morphisms of
functors](https://en.wikipedia.org/wiki/Natural_transformation). Nothing complex,
just arrows everywhere.

## Algebras

Algebras are a collection of functions operating over some ADTs, along with a
set of laws or constraints specifying relationships between these functions. For
example, the [magma](https://en.wikipedia.org/wiki/Magma_(algebra)) is defined
as a set that is [closed](https://en.wikipedia.org/wiki/Closure_(mathematics))
under a binary operator. Functional programmers create
algebras to formalize and verify the design of their programs.

All this "complex" algebraic stuff is actually what you learned in elementary school:

* A [semigroup](https://en.wikipedia.org/wiki/Semigroup) is a magma where the binary operator is [associative](https://en.wikipedia.org/wiki/Associative_property).
* A [monoid](https://en.wikipedia.org/wiki/Monoid) is a semigroup where the set has a [identity element](https://en.wikipedia.org/wiki/Identity_element).
* A [group](https://en.wikipedia.org/wiki/Group_(mathematics)) is a monoid where every element of the set has a unique [inverse element](https://en.wikipedia.org/wiki/Inverse_element).
* An [Abelian group](https://en.wikipedia.org/wiki/Abelian_group) is a group where the binary operator is also [commutative](https://en.wikipedia.org/wiki/Commutative_property).
* A [semiring](https://en.wikipedia.org/wiki/Semiring) is an algebraic structure with set R and 2 binary operators, + and ⋅, called
  addition and multiplication. It has the following laws:
    * (R, +) is a commutative monoid with identity of 0.
    * (R, ⋅) is a monoid with identity of 1.
    * Multiplication is left- and right-[distributive](https://en.wikipedia.org/wiki/Distributive_property).
    * Multiplication with 0 annihilates R.
* In a [ring](https://en.wikipedia.org/wiki/Ring_(algebra)), the set has an [additive inverse](https://en.wikipedia.org/wiki/Additive_inverse).

## Abstractions Based on ADTs

So, you decided to create some abstractions by composing some functions that
accept and return some ADTs possibly creating some effects. Here you are
entering the territory of the functional plumbing using morphisms. The
constraints imposed by the static type checking could not be satisfied automatically.
Let's assume that `mayb` returns an integer result of some
computation wrapped in `Maybe`.

```haskell
mayb x = Just x -- here we just wrap x of any type into Maybe
```

Let's create an abstraction `addcomp`, that adds an Int to the result of `mayb`.

```haskell
addcomp :: Int -> Maybe Int
addcomp y = y + mayb 1 -- this does not work
```

You can not say that `y + mayb 1` because there is no `+` operator that sums
integers and `Maybe`. In OOP you probably would implement such an operator or a
visitor. In FP you need to use a functor (<$>) to perform addition inside the
context of `Maybe`.

```haskell
addcomp y = (+ y) <$> mayb 1 -- here + is partially applied
```

To use the functor or another morphism with your own wrapper type,
it is necessary to implement the corresponding typeclass by providing instances of
its methods such as `map`, `fmap`, `mappend` and others. For example, this is
how `fmap` of the functor typeclass instance for `Maybe` is implemented:

```haskell
instance Functor Maybe where    -- <$> seen above is the infix synonym for fmap
  fmap f Nothing = Nothing      -- nothing is done with Nothing
  fmap f (Just x) = Just (f x)  -- x is destructured from Maybe, applied to f,
                                -- and placed back
```

`fmap` takes a bare function, `+` in our case, along with a value wrapped into a
context, and applies the function inside the same context. 

Sometimes computations end with functions packed inside a context. In this case,
we use [applicative functor](https://en.wikipedia.org/wiki/Applicative_functor)
(<*>) for plumbing. This appliance accepts the function inside a context, a
value inside a context, takes them out, applies them, and puts the result back.
For the `Maybe` container `pure 1` returns `Just 1`. Watch the hands:

```haskell 
addcomp y = mayb (+ y) <*> pure 1 -- this implementation of addcomp produces 
                                  -- the same result as above 
```

All compositional operators in Haskell are intended for such plumbing. Your
task is to match the input types of the composed functions with their output types.
To become a skilled functional plumber, it is necessary to understand the
meaning of the bottomless crevasse of such magical words as functor, bifunctor,
profunctor, applicative functor, invariant, contravariant, lens, Kleisli arrow,
monad, and, save the Lord, comonad. All plumbing should then be checked for
consistency with the corresponding algebraical laws of the entities
mentioned above. Do the used functions commute? Does the identity law hold?
There is even some [advanced
literature](https://github.com/BartoszMilewski/Publications/blob/master/TheDaoOfFP/DaoFP.pdf)
on this topic that promises a path to the functional nirvana.

## Monads

No text on functional programming could avoid the topic of monads. Monads
are truly magical. They allow chaining sequential computations that produce effects.
They even allow performing side effects or input/output, which are considered
impure operations.

What are [monads](https://en.wikipedia.org/wiki/Monad_(functional_programming))?
Monads are monoids in the category of endofunctors, which, as you remember, are
semigroups with identity. This means that they are also functors. Particularly,
applicative ones. If this does not make things clear, monads just allow to
compose functions of the form `f: a -> m b`, which are called monadic or
effectful functions. Here `m` is a context for which you need to implement a
monad typeclass instance, and, as the signature suggests, the type `b` may be
completely different from the type `a`. For an example, let's create a monadic
function that returns the reciprocal of the given number:

```haskell 
reciprocal :: Int -> Maybe Double
reciprocal 0 = Nothing
reciprocal x = Just $ 1 / (fromIntegral x)
```

Because the `addcomp` function from the example above is also monadic, we 
can compose them using the monadic bind (>>=): 

```haskell
moncomp = addcomp 1 >>= reciprocal -- in the point-free notation 
                                   -- the argument is assumed
```

The call `moncomp 1` will result in `Just 0.5`.

All the magic happens in the context's implementation of bind. 
This is how bind for `Maybe` is implemented:

```haskell
instance Monad Maybe where  
...
(>>=) :: m a -> (a -> m b) -> m b  
Just x >>= f = f x  
```

It takes a value in a context and a monadic function, so the result remains in
the context. The use of monads may bring its own problems. The contexts may stack
like onion petals, and additional plumbing may be required to peel them off.

# Conclusion

While OOP programmers just declare properties in their classes, to maintain a state,
FP purists are bound to pass monads in and out. They usually stash the state somewhere
in tuples along with the results of their computations. Does this help to create
clear, comprehensible, and maintainable designs? It is for you to decide. Also, note
that users of the dynamically-typed functional languages do not learn category
theory and just use the `comp` function to compose. It is barely imaginable, 
how they are able to write working programs with this.

