# How to render a machine

The other operation that we might be interested in performing when we have a `machine :: StateMachine a b` is obtaining a graphical representation of it to understand better how it works and maybe using it as documentation and/or as a starting point for a discussion with a domain expert.

Currently, there are two representations which `crem` is able to create.

## State space rendering

The first representation which you can obtain it the representation as a graph of the topology of a machine.

The only supported output format for the moment is [Mermaid](https://mermaid.js.org), and you can produce it composing the `machineAsGraph` and `renderUntypedGraph` functions from the [`Render`](/src/Crem/Render/Render.hs) module.

For example, this is the graph produced for the [`LockDoor`](/src/Crem/Example/LockDoor.hs) machine

<img alt="lock door machine" src="https://mermaid.ink/svg/pako:eNptkEFPhDAQhf8KeWcggN2F9uBFPZhoPHgzvTR0VLJLS0oxroT_bhdEd-MeJpmX-eZl5o2orSYI9F55um3Um1Nt8lFIc98_2Hr31JFZ-5u97Umv6lh_6shFSXIdXUIXdTL-73oyPDde1AVrxGjJtarR4fpRmiiS8O_UkoQIrVZuJyHNFLih0-G5O9146yBe1b6nGGrw9vlgagjvBlqhnwR-qU6ZF2vPNMSITwiWbdJtVfEtq9hVuYlxgCjyMmWc84qVLC_yjLMpxte8n6UBofmExyXxOfjpGxvJf_Y" height=150px style="display: block; margin: auto;">

## Flow rendering

Rendering the state space for big machines could be quite uninformative, because it could easily get quite big.

A possibly more useful representation is provided by the flow representation (i.e. a graph showing the state space of each `Basic` machine and how they were composed) which can be generated using the `renderFlow` function from the [`RenderFlow`](/src/Crem/Render/RenderFlow.hs) module.

To be called, it requires us to annotate every leaf of our tree of machines with a label, which will be used in the drawing.

Then it will produce a diagram like

![risk manager flow](https://mermaid.ink/svg/pako:eNqlVF1PwjAU_SvkPmkyCGCBbQ8mCvqkxkj0wSwhzXad1a4lXUdAsv9u9-HcCMgMfepOz73n3PbubsGXAYILsaYaZ4yGikbd1dATHbNysEPDUGGY7bYFnK0KXDzIGdX0BZXG9b7zqeQcfY3Bc4yqJfVOUjFDTRmPb5mK9eGQJ_SRrTCYKgyYvk4U0iQTORJWKV1x_renenmdbvfyxMqa1EMJ29ffLvGpt7TfzyH3R--0hZ1_5E7rzbqUnPmbeqcWyOLsvMH-bepMqOAUJ2WGhn5DQckPY4VJ0VCp0MWciZDjYwXs8bqjXlHrOpEvhU91XaSEdmupOcrSlSywIEIVURaY3ztP4oF-xwg9cM02oOrTA0-khpcsA6N4Y95CKnDfKI_RAppoOd8IH1ytEvwhlSOiYi2peJWy8Q3uFtbgkv6oN7ZtZ0xscjEZWbABdziY9IjjODaZkMFw0HdIasFXHt_vGQrmFu6LkZRPpvQbS36R_g)

which will show the flow of the composed machine and, in every box, the state space representation for every single composed machine.
