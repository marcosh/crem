# How to compose machines

One of the perks of using `crem` is that it is compositional and allows creating complex machines out of simpler ones.

There are three sets of combinators that you can use to compose your state machines.

The lower level one is given by the several `StateMachine` constructors, which have already been described in [How to create a machine](how-to-create-a-machine.md). They provide the most complete set of operations to compose state machines. On the other hand, some of them are quite ad-hoc and possibly temporary.

The other two live at a higher level and are provided by the `Category`/`Profunctor` hierarchy and the `Arrow` hierarchy. Let's look at them in more detail.

## [`Category`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Category.html)

The `Category` type class provides us a notion of composition.

Concretely, for a type `p` with an instance of `Category`, it consists of:

- for every type `a`, an identity value `id` of type `p a a`.
- a function `(.) :: p b c -> p a b -> p a c` which allows composing values with aligning types. It needs to be associative and `id` should act as an identity both on the left and on the right.

In the context of `StateMachine`s, the `id` state machine is just the machine with a single state which emits every input it receives.

The `(.)` function is implemented as sequential composition. First a machine is executed, and every output is feed as input to the second machine.

## [`Profunctor`](https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor.html#t:Profunctor)

A `Profunctor` instance on `StateMachine` allows to pre-compose or post-compose a `StateMachine` with a normal pure function.

Notice that post-composing (i.e. acting on outputs) is covariant

```haskell
rmap :: (a -> b) -> StateMachine c a -> StateMachine c b
```

while pre-composition (i.e. acting on inputs) is contravariant

```haskell
lmap :: (a -> b) -> StateMachine b c -> StateMachine a c
```

## [`Strong`](https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor.html#t:Strong)

An instance of the `Strong` type class on a type `p` means basically that `p` behaves nicely with respect to tuples.

The basic operations of the type class allow pairing a process `p a b` with another value `c`

```haskell
first' :: p a b -> p (a, c) (b, c)

second' :: p a b -> p (c, a) (c, b)
```

From these basic operations, when `p` is also a `Category`, some fairly interesting ones could be implemented, as

```haskell
splitStrong :: p a b -> p c d -> p (a, c) (b, d)
```

which, for state machines, represents parallel execution of machines.

We could also have

```haskell
fanOut :: p a b -> p a c -> p a (b, c)
```

which feeds the same input to multiple machines and collects all the outputs.

## [`Choice`](https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor.html#t:Choice)

An instance of the `Choice` type class on a type `p` means basically that `p` behaves nicely with respect to `Either`.

The basic operations of the type class allow adding an alternative value `c` to a process `p a b`

```haskell
left' :: p a b -> p (Either a c) (Either b c)

right' :: p a b -> p (Either c a) (Either c b)
```

From these basic operations, when `p` is also a `Category`, some fairly interesting ones could be implemented, as

```haskell
splitChoice :: p a b -> p c d -> p (Either a c) (Either b d)
```

which, for state machines, represents alternative execution of machines, depending on which input is provided.

We could also have

```haskell
fanIn :: p a c -> p b c -> p (Either a b) c
```

which allows combining multiple machines with the same output but different inputs.

## [`Arrow`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Arrow.html#t:Arrow)

The `Arrow p` class describes computations similarly to what `(Category p, Strong p)` do. The main difference is that is also requires that every function `a -> b` could be interpreted as `p a b`.

Its basic operations are

```haskell
arr :: (a -> b) -> p a b

first :: p a b -> p (a, c) (b, c)
```

Other interesting combinators, analogous to `splitStrong` and `fanOut` are

```haskell
(***) :: p a b -> p c d -> p (a, c) (b, d)

(&&&) :: p a b -> p a c -> p a (b, c)
```

When using the `Arrow` abstraction it is also possible to use the special [`proc` notation](https://www.haskell.org/arrows/syntax.html), which allows to use a syntax similar to the one introduced by `do` notation for monads.

## [`ArrowChoice`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Arrow.html#t:ArrowChoice)

The `ArrowChoice` expands `Arrow` providing conditional execution, similarly to what `Choice` does for profunctors.

Its basic operation is

```haskell
left :: p a b -> p (Either a c) (Either b c)
```

Similarly to what is possible for `Choice` we can define also combinators

```haskell
(+++) :: p a b -> p c d -> p (Either a c) (Either b d)

(|||) :: p a c -> p b c -> p (Either a b) c
```
