> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE UndecidableInstances #-}
> -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wmissing-deriving-strategies
> {-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
> -- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wunused-type-patterns
> {-# OPTIONS_GHC -Wno-unused-type-patterns #-}
>
> module Crem.Example.TwoSwitchesGate where
>
> import "crem" Crem.BaseMachine
> import "crem" Crem.StateMachine
> import "crem" Crem.Topology
> import "profunctors" Data.Profunctor
> import "singletons-base" Data.Singletons.Base.TH

We would like to implement a gate opening mechanism controlled by two switches. We would like the gate to open only when the two switches are on.

We would like to implement this by composing several small state machines: one for every switch, one for making sure that we actually receive the right message from both switches, and one for actually opening the gate.

Let's start with the switch.

The first thing we need to do is to define the topology of our machine, meaning the allowed transitions in its state space.

For a switch, there are only two states. Either the switch is on or it is off.

Moreover, we want those switches to be usable only once, and therefore we want to forbid the transition from the `on`` to the `off` position. In other terms, we allow only to go from the `off` position to the `on` position.

> $( singletons
>     [d|
>       data SwitchVertex
>         = SwitchIsOn
>         | SwitchIsOff
>         deriving stock (Eq, Show, Bounded, Enum)
>
>       switchTopology :: Topology SwitchVertex
>       switchTopology = Topology
>         [(SwitchIsOff, [ SwitchIsOn ])]
>     |]
>  )

Notice that we need to wrap this in `singletons` because we will soon need to use this data type as a kind, to store information in the type of our state machines.

Next we need to define which data every vertex of our topology should contain. To express that we use a generalized algebraid data type indexed with `SwitchVertex`

> data SwitchState (vertex :: SwitchVertex) where
>   OnState :: SwitchState 'SwitchIsOn
>   OffState :: SwitchState 'SwitchIsOff

In this case, for every vertex there is just one possible state.

At this point we need to define which inputs our machine should handle and which outputs it should emit. In the case there is only one meaningful input, the request of turning on the switch, and one meaningful output, the notification that the switch has been turned on.

> data SwitchInput = TurnOn
>
> data SwitchOutput = TurnedOn

At this point we can actually implement our switch as a `BaseMachine`

> switch :: () -> BaseMachine SwitchTopology SwitchInput SwitchOutput
> switch _ =
>   BaseMachineT
>     { initialState = InitialState OnState
>     , action = \case
>         OnState -> \_ -> pureResult TurnedOn OnState
>         OffState -> \_ -> pureResult TurnedOn OnState
>     }

We start from the `OnState` and every time we receive a request to turn the switch on, we return a message informing the external world that the switch in turned on and we update the state accordingly if needed.

Since we need two separate switches, we can create them by invoking the `switch` function twice

> switch1 :: BaseMachine SwitchTopology SwitchInput SwitchOutput
> switch1 = switch ()
>
> switch2 :: BaseMachine SwitchTopology SwitchInput SwitchOutput
> switch2 = switch ()

This concludes the implementation of our switch machine. Next, we would like to implement a machine which receives as inputs the output of two switches and emits a message whenever both the switches have been turned on.

Again, we need to start thinking about the topology of our machine. Since we need to track the state of the two switches, we will have four vertices

> $( singletons
>     [d|
>       data BothVertex
>         = NoSwitchOn
>         | OnlyFirstSwitchOn
>         | OnlySecondSwitchOn
>         | BothSwitchesOn
>         deriving (Eq, Show, Enum, Bounded)
>
>       bothTopology :: Topology BothVertex
>       bothTopology = Topology
>         [ (NoSwitchOn, [OnlyFirstSwitchOn, OnlySecondSwitchOn])
>         , (OnlyFirstSwitchOn, [BothSwitchesOn])
>         , (OnlySecondSwitchOn, [BothSwitchesOn])
>         ]
>     |]
>  )

The topology again constrains the machine with the invariant the we can only turn on switches.

Next we need to define the state space, assigning a data type to every vertex in the topology. In this case we don't have the need to attach data to our vertices so we can simply define

> data BothState (vertex :: BothVertex) where
>   NoSwitchOnState :: BothState 'NoSwitchOn
>   OnlyFirstSwitchOnState :: BothState 'OnlyFirstSwitchOn
>   OnlySecondSwitchOnState :: BothState 'OnlySecondSwitchOn
>   BothSwitchesOnState :: BothState 'BothSwitchesOn

Before defining the logic of the machine, we need to define its inputs and outputs. Since we would like it to monitor the outputs of both switches, its input type could be

> type BothInput = Either SwitchOutput SwitchOutput

Its output instead will be a potential message to actually open the gate

> data OpenGate = OpenGate
>
> type BothOutput = Maybe OpenGate

and eventually we can define the logic of our state machine

> bothMachine :: BaseMachine BothTopology BothInput BothOutput
> bothMachine =
>   BaseMachineT
>     { initialState = InitialState NoSwitchOnState
>     , action = \case
>         NoSwitchOnState -> \case
>           Left _ -> pureResult Nothing OnlyFirstSwitchOnState
>           Right _ -> pureResult Nothing OnlySecondSwitchOnState
>         OnlyFirstSwitchOnState -> \case
>           Left _ -> pureResult Nothing OnlyFirstSwitchOnState
>           Right _ -> pureResult (Just OpenGate) BothSwitchesOnState
>         OnlySecondSwitchOnState -> \case
>           Left _ -> pureResult (Just OpenGate) BothSwitchesOnState
>           Right _ -> pureResult Nothing OnlySecondSwitchOnState
>         BothSwitchesOnState -> \_ -> pureResult Nothing BothSwitchesOnState
>     }

The last machine that we need is one representing the actual gate. Since the logic is exactly the same as the one of the switches, we can actually reuse what we defined above

> gate :: BaseMachine SwitchTopology SwitchInput SwitchOutput
> gate = switch ()

Now we have all the machines we wanted and we need to connect them appropriately.

We have the two switches which produce a `SwitchOutput` and the `bothMachine` which accepts inputs of type `Either SwitchOutput SwitchOutput`.

We need to pair up the two switches, first, and then connect them to the `bothMachine`. We need to pair the two switches in a way that allows us to decide whether to run one or the other: this is exactly what the `Alternative` constructor of the `StateMachineT` data type allows us to do.

> switches :: StateMachine (Either SwitchInput SwitchInput) (Either SwitchOutput SwitchOutput)
> switches = Basic switch1 `Alternative` Basic switch2

Notice that we had to wrap our `switch` machines with `Basic` to turn them into `StateMachine`s, which is the more composable type used by `Alternative`.

Now we have the output of `switches` which coincides with the input of `bothMachine`, and therefore we can pass every output we get from `switches` to `bothMachine`. We use the `Sequential` constractor exactly for this

> bothSwitches :: StateMachine (Either SwitchInput SwitchInput) BothOutput
> bothSwitches = switches `Sequential` Basic bothMachine

Now we have a machine which emits `BothOutput = Maybe OpenGate`. Our `gate` machine on the other hand accepts inputs of type `SwitchInput`. To connect those, we need to do some adjusting.

First, we can translate an `OpenGate` into a `SwitchInput`

> openGateToSwitchInput :: OpenGate -> SwitchInput
> openGateToSwitchInput OpenGate = TurnOn

and we can use this function to adapt our `gate` machine so that it accepts `OpenGate` as input.

> gate' :: BaseMachine SwitchTopology OpenGate SwitchOutput
> gate' = lmap openGateToSwitchInput gate

Still `bothSwitches` emits values of type `Maybe OpenGate`. We could lift our `gate'` machine to `Maybe OpenGate` inputs using the `maybeM` combinator.

> maybeGate :: BaseMachine SwitchTopology (Maybe OpenGate) (Maybe SwitchOutput)
> maybeGate = maybeM gate'

At this point we could conclude our composition, joining together `bothMachine` and `maybeGate`

> gateMachine :: StateMachine (Either SwitchInput SwitchInput) (Maybe SwitchOutput)
> gateMachine = bothSwitches `Sequential` Basic maybeGate