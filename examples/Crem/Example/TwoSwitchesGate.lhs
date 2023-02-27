> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE OverloadedStrings #-}
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
> import "crem" Crem.Render.Render
> import "crem" Crem.Render.RenderFlow
> import "crem" Crem.StateMachine
> import "crem" Crem.Topology
> import "base" Data.Functor.Identity
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
>   deriving stock Show
>
> instance Semigroup SwitchOutput where
>   TurnedOn <> TurnedOn = TurnedOn

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

Now we have a single machine which describes out whole flow.

Now, there are two things which we could do with `gateMachine`.

The first thing is actually executing it. To do it we can use the `runMultiple` function.

We can try to to turn on both switches and verify that the gate actually opened

> -- |
> -- >>> openedGate
> -- Just TurnedOn
> openedGate :: Maybe SwitchOutput
> openedGate = fst . runIdentity $ runMultiple gateMachine [Left TurnOn, Right TurnOn]

Or we can turn just the first switch several times without opening the gate

> -- |
> -- >>> closedGate
> -- Nothing
> closedGate :: Maybe SwitchOutput
> closedGate = fst . runIdentity $ runMultiple gateMachine [Left TurnOn, Left TurnOn, Left TurnOn]

The other thing we can do is actually rendering a diagram representing how the `gateMachine` works.

The best rendering we can get displays the flow of the machine and the state space for every step of the flow

> -- |
> -- >>> gateFlow
> -- Right "state switch1 {\nswitch1_SwitchIsOn\nswitch1_SwitchIsOff\nswitch1_SwitchIsOff --> switch1_SwitchIsOn\n}\nstate switch2 {\nswitch2_SwitchIsOn\nswitch2_SwitchIsOff\nswitch2_SwitchIsOff --> switch2_SwitchIsOn\n}\nstate fork_choice_switch1switch2 <<choice>>\nstate join_choice_switch1switch2 <<choice>>\nfork_choice_switch1switch2 --> switch1\nfork_choice_switch1switch2 --> switch2\nswitch1 --> join_choice_switch1switch2\nswitch2 --> join_choice_switch1switch2\nstate both {\nboth_NoSwitchOn\nboth_OnlyFirstSwitchOn\nboth_OnlySecondSwitchOn\nboth_BothSwitchesOn\nboth_NoSwitchOn --> both_OnlyFirstSwitchOn\nboth_NoSwitchOn --> both_OnlySecondSwitchOn\nboth_OnlyFirstSwitchOn --> both_BothSwitchesOn\nboth_OnlySecondSwitchOn --> both_BothSwitchesOn\n}\njoin_choice_switch1switch2 --> both\nstate gate {\ngate_SwitchIsOn\ngate_SwitchIsOff\ngate_SwitchIsOff --> gate_SwitchIsOn\n}\nboth --> gate"
> gateFlow :: Either String Mermaid
> gateFlow = (\(mermaid, _, _) -> mermaid) <$>
>   renderFlow
>     (BinaryLabel
>        (BinaryLabel
>           (BinaryLabel
>             (LeafLabel "switch1")
>             (LeafLabel "switch2"))
>           (LeafLabel "both"))
>        (LeafLabel "gate"))
>     (gateMachine @Identity)

The result is a diagram which looks like [this](https://mermaid.live/edit#pako:eNqNVN9vgjAQ_lfIPYORCgjE-LBsS_aw-eDbQkI6KMKU1kDd5oz_-0qhcwyr8tAf331333G99gAJSwmEUHPMyX2BVxUurQ8UUQkY9WfBk9w2DhE1xNdt46Wcn-oF1eBZpjUYljU_G-jYF0V9UaQRRTpRpBFF50UzVq3jJGdFQuIuO5XHbNbi87kiv7OC3kC-EPNPEW7kNWfSnUYD6lNQPHSdJ3_mjfFc1bpZxy-srZAqtAQXdLN_LKqaa2xLkjCanjHeiaGFSd0znGRkoldVdHyt8iDYyU-b1DCg3kn0zoVGUG6qzKtm6MrcrAf93AdVM_9HZdyBv0hFHqMyggklqUpcpOJuS9EIeE5KEkEolimu1hEIN8HbbVPh8JAWnFUQZnhTExPwjrPlniYQ8mpHFKl7H35ZW0xfGevtITzAF4RWYAfeaDwOpp7rIdeEPYROgEaub6PA95GY3KMJ39I7GE0dx5-4E9vxJv50LOhEpvPcvk3yiTr-AO8tobo) where you can clearly see the overall structure of the machine we created, and for every step of the flow the state space of the basic state machine governing that step.
