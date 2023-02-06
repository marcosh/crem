# How to run a machine

Once you have created your cute `StateMachine a b`, there are two things you could do with it using `crem`. One thing is running it, providing inputs and receiving outputs. The other is rendering its topology. In this section we will concentrate on the first aspect, the second will be material for the next section.

## The `run` operation

Given a `machine :: StateMachine input output`, the basic operation there is available to execute it is

```haskell
run :: StateMachine a b -> a -> (b, StateMachine a b)
```

Given a state machine and an input value, we perform one transition of the machine to retrieve the emitted output and a new version of the machine (i.e. the same machine but with a different internal state).

## The `runMultiple` operation

If we want to provide to our machine several inputs to be processed sequentially, we can use

```haskell
runMultiple
  :: Monoid b
  => StateMachine a b -> [a] -> (b, StateMachine a b)
```

It allows us to provide a list of inputs and retrieve an output and a new version of machine itself.

The output needs to have a `Monoid` instance, since every step of the machine execution will produce an output. Choosing wisely the monoid instance allows us to use only the last output or conversely collect all of them.
