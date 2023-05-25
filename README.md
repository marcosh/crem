# [crem](https://github.com/tweag/crem)

<img src="https://raw.githubusercontent.com/tweag/crem/main/logo/crem-transparent.png" width="200">

![CI status](https://github.com/tweag/crem/actions/workflows/ci.yml/badge.svg)

`crem` stands for **c**ompositional **r**epresentable **e**xecutable **m**achines.

It allows defining state machines (Mealy machines in fact), composing them to build bigger machines out of smaller ones and then running them and drawing their flow and their state space.

## What can you do with `crem`

### Defining state machines

`crem` allows you to define state machines so that you can enforce which state transitions are actually allowed by your machine.

If you try to implement a machine by running a transition which is not allowed, you will get a compilation error.

More details on how to define a machine at [How to create a machine](/docs/how-to-create-a-machine.md).

### Composing state machines

`crem` allows you to compose machines together to build more complex ones, proving a compositional language to implement state machines.

More details on how to compose machines at [How to compose machines](/docs/how-to-compose-machines.md)

### Rendering a state machine

Thanks to the information on the allowed transitions `crem` collects when you define a machine, it is able to produce a graphical representation of the flow and the state space of your machine.

One example of such an output is

![risk manager flow](https://mermaid.ink/svg/pako:eNqlVF1PwjAU_SvkPmkyCGCBbQ8mCvqkxkj0wSwhzXad1a4lXUdAsv9u9-HcCMgMfepOz73n3PbubsGXAYILsaYaZ4yGikbd1dATHbNysEPDUGGY7bYFnK0KXDzIGdX0BZXG9b7zqeQcfY3Bc4yqJfVOUjFDTRmPb5mK9eGQJ_SRrTCYKgyYvk4U0iQTORJWKV1x_renenmdbvfyxMqa1EMJ29ffLvGpt7TfzyH3R--0hZ1_5E7rzbqUnPmbeqcWyOLsvMH-bepMqOAUJ2WGhn5DQckPY4VJ0VCp0MWciZDjYwXs8bqjXlHrOpEvhU91XaSEdmupOcrSlSywIEIVURaY3ztP4oF-xwg9cM02oOrTA0-khpcsA6N4Y95CKnDfKI_RAppoOd8IH1ytEvwhlSOiYi2peJWy8Q3uFtbgkv6oN7ZtZ0xscjEZWbABdziY9IjjODaZkMFw0HdIasFXHt_vGQrmFu6LkZRPpvQbS36R_g)

More details on how to render a machine at [How to render a machine](/docs/how-to-render-a-machine.md)

### Running a machine

Last but not the least, you can also execute a machine, providing inputs to it and receiving the emitted outputs.

More details on how to run a machine at [How to run a machine](/docs/how-to-run-a-machine.md)

### Want to know more?

Further documentation can be found in the [docs](/docs) folder.

The [examples](/examples) folder contains a lot of examples, from simple machines to complex ones describing entire workflows.

I would recommend to check:

- [TwoSwitchesGate.lhs](/examples/Crem/Example/TwoSwitchesGate.lhs) if you want to see all the code which is needed to use the library with a quite detailed explanation.
- [RiskManager](/examples/Crem/Example/RiskManager/) if you want to see how to use `crem` to [model your domain using state machines](http://marcosh.github.io/post/2021/10/27/ddd-state-machines.html) following the ideas coming from Domain Driven Design.
- [Uno.hs](/examples/Crem/Example/Uno.hs) if you want to see how to structure a card game like [Uno](https://en.wikipedia.org/wiki/Uno_(card_game)), with an implementation ported from [UnoCore](https://github.com/thinkbeforecoding/UnoCore/blob/solution/Uno/Game.fs) by [@thinkb4coding@mastodon.social](https://functional.cafe/@thinkb4coding@mastodon.social).

Be sure to check out also the [spec](/spec) folder, where all the tests of the application are included, to see in practice what you can do with `crem`.

## Development

This is a Haskell Cabal project that uses Nix for development. Nix is optional but recommended.

### environment

A Nix shell is available with all the required tools. To enter the shell, issue

```sh
# default GHC
nix develop

# custom GHC
nix develop .#ghc90
nix develop .#ghc92
```

Or, without flakes:

```sh
# default GHC
nix-shell
```

### GHC version

The project has a default GHC version that is specified in the flake. At the moment that version is 9.0, because the HLS plugin Wingman [currently only builds up to this version](https://github.com/haskell/haskell-language-server/issues/2971).

It is also possible to use other GHC versions to build the project and enter development shells. This allows us to easily test multiple versions.

### Building

In a development shell, you can simply build the project with Cabal:

```sh
cabal build
```

This provides us fast incremental builds, ease of debugging, etc.

Inside the development shell, you can also use the commands

```sh
# just build the project
build-watch

# execute also the tests
test-watch
```

### Haddock documentation

You can generate and see the [Haddock] documentation by running

```sh
cabal haddock --open
```

### Code formatting

Code is formatted using [fourmolu](https://github.com/fourmolu/fourmolu) version 0.12.0.0.

### Cabal flags

We have a `cabal` flag called `errors` which allows enabling `-Werror`. It has a default of `False`, so that warning are not turned into errors.

In development and CI we use the flag `-f errors` so that we can avoid any warning in the library code.

### Changelog

All changes are tracked in the [`Changelog`](CHANGELOG.md).

## Project setup

You can find more details on the project setup on [Setup.md](/Setup.md).

## Known limitations

The project is still in its early stage and not everything is crafted to perfection.

Some known limitations to the current state of the project are:
- `crem` has not been tested on huge state machines. Compilation times might grow very rapidly.
- the topologies which `crem` allows you to define need to be finite, and not particularly big, either. For example, it is not feasible to use `Int` as the type of vertices for a topology.
- in its current state, the `StateMachine` type is not extensible. It has a predefined set of constructors, but maybe there are more which make sense and are not present yet.

Moreover, for current bugs and feature requests, you can check the [open issues](https://github.com/tweag/crem/issues).

## Credits

![tweag logo](./logo/tweag.png)

The initial development of `crem` was funded by [Tweag](https://www.tweag.io/).

## Contributions

Contributions are extremely welcome. If you have any idea on how to improve the library, its code or its documentation, feel free to open an [issue](https://github.com/tweag/crem/issues), create a [pull request](https://github.com/tweag/crem/pulls), or just contact directly one of the maintainers.

## Logo

The `crem` logo was kindly generated by [craiyon](https://www.craiyon.com/).

## Resources

If you want to know a bit more of `crem` and some ideas behind it, here are some additional resources you can check out:

- [Domain modelling with state machines](http://marcosh.github.io/post/2021/10/27/ddd-state-machines.html), the first blog post where I started considering using composable state machines for DDD-like architectures.
- [Composable Haskell state machines with `crem`](https://www.youtube.com/watch?v=cvbOG1I6wrU), if you prefer a video introducing `crem`.
- [State machines with `crem`](https://hackmd.io/@CJO5VbycTsyzjGBytbwezQ/rkJliIjRj#/) the slide deck used for the above presentation.
- [Crem: compositional representable executable machines](https://www.tweag.io/blog/2023-04-13-crem-state-machines/), a blog post explaining the main ideas of `crem`.
- [Domain modelling with state machines](https://www.youtube.com/watch?v=UBnHpnss8Fg), a video describing the main ideas motivating `crem`.
