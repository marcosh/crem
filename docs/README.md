# `Crem` docs

`Crem` is a library to create, compose, run and render state machines.

Usually the library should be used following these four steps:

- [create several small machines](./how-to-create-a-machine.md)
- [compose the machines together to create more complex machines](./how-to-compose-machines.md)
- [execute your machine](./how-to-run-a-machine.md)
- [render the topology and the flow of your machine](./how-to-render-a-machine.md)

Notice that throughout the documentation we will work only with pure machines, while `crem` itself supports machines performing effects in a context described by a monad `m`.
