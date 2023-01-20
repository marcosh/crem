# How to create a machine

A `StateMachine a b` is a stateful process which receives inputs of type `a` and emits outputs of type `b`.

We will represent a machine as such

```
a ┌────┐ b
──┤    ├──
  └────┘
```

as black boxes, where inputs are on the left and outputs are on the right.

The `StateMachine a b` data type has four constructors which we can use to construct a machine:

- `Basic`
- `Compose`
- `Parallel`
- `Alternative`

Let's start with the last three.

## `Compose`

```haskell
Compose
  :: StateMachine a b
  -> StateMachine b c
  -> StateMachine a c
```

allows us to sequentially compose two machines

```
a ┌────┐ b   b ┌────┐ c
──┤    ├──   ──┤    ├──
  └────┘       └────┘
```

to get a single machine

```
  ┌─────────────────┐
a │ ┌╌╌╌╌┐ b ┌╌╌╌╌┐ │ c
──┼╌┤    ├╌╌╌┤    ├╌┼──
  │ └╌╌╌╌┘   └╌╌╌╌┘ │
  └─────────────────┘
```

where every output `b` of the first machine is passed as an input to the second machine.

## `Parallel`

```haskell
Parallel
  :: StateMachine a b
  -> StateMachine c d
  -> StateMachine (a, c) (b, d)
```

allows us to execute two machines in parallel

```
a ┌────┐ b
──┤    ├──
  └────┘
c ┌────┐ d
──┤    ├──
  └────┘
```

to get a single machine

```
       ┌────────────┐
       │ a ┌╌╌╌╌┐ b │
       │┌╌╌┤    ├╌╌┐│
(a, c) ││  └╌╌╌╌┘  ││ (b, d)
───────┼┤          ├┼───────
       ││  ┌╌╌╌╌┐  ││
       │└╌╌┤    ├╌╌┘│
       │ c └╌╌╌╌┘ d │
       └────────────┘
```

which passes the first element of the input tuple to the first machine and the second element to the second machine, collects the outputs and emits them together in a tuple.

## `Alternative`

```haskell
Alternative
  :: StateMachine a b
  -> StateMachine c d
  -> StateMachine (Either a c) (Either c d)
```

allows us to execute one out of two machines, depending on the input.

If we have two machines

```
a ┌────┐ b
──┤    ├──
  └────┘
c ┌────┐ d
──┤    ├──
  └────┘
```

we can compose them like so

```
           ┌────────────┐
           │ a ┌╌╌╌╌┐ b │
           │┌╌╌┤    ├╌╌┐│
Either a c ││  └╌╌╌╌┘  ││ Either b d
───────────┼┤    ⊕    ├┼───────────
           ││  ┌╌╌╌╌┐  ││
           │└╌╌┤    ├╌╌┘│
           │ c └╌╌╌╌┘ d │
           └────────────┘
```

where the `⊕` symbol allows us to distinguish alternative composition from the parallel one.

In practice, if the composed machine receives a `Left a` as input, the `a` will be passed as input to the first machine that will emit a `b`, which will be wrapped to emit a `Left b` from the composed machine, while the second machine remains untouched.
Similarly, if the composed machine receives a `Right c` as input, the `c` will be passed as input to the second machine that will emit a `d`, which will be wrapped to emit a `Right d` from the composed machine, while the first machine remains untouched.

## `Basic`

All the other constructors are combinators to construct more complicated machine out of simpler ones. We still miss a way to build the simpler ones! That is exactly what the `Basic` constructor provides. It gives a way to create a machine by specifying precisely its internal behaviour.

```haskell
Basic
    :: forall vertex (topology :: Topology vertex) a b
     . ( Demote vertex ~ vertex
       , SingKind vertex
       , SingI topology
       , Show vertex
       )
    => BaseMachine topology a b
    -> StateMachine a b
```

To build a `StateMachine a b` using the `Basic` constructor, we need to build first a `BaseMachine topology a b` satisfying some constraints.

Let's start by understanding what `topology` represents.

### `Topology`

The `Topology` data type is defined as

```haskell
newtype Topology vertex = Topology
  {edges :: [(vertex, [vertex])]}
```

and represents a directed graph on a set of elements of type `vertex`.

It is indexed by a `vertex` type, and it is a newtype wrapper around a list, where every element of the list is a pair containing an element `a :: vertex` and a list of elements of type `vertex`. Every element `b :: vertex` in this last list means that there is an edge from `a` to `b`.

In practice, a `Topology` is a list of pairs, where every pair contains a vertex `a` and the list of vertices where an edge coming from `a` ends.

In fact, we use [`singletons`](https://hackage.haskell.org/package/singletons) to promote `Topology` to the type level and use it to keep track of the available machine transitions at the type level.

We use a `Topology vertex` kind, and we construct types of that kind using the `Topology :: [(vertex, [vertex])] -> Topology vertex` type constructor. In such a use, notice that also `vertex` is a kind.
