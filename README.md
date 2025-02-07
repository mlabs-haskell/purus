# purus

Purus is a work-in-progress fork of PureScript that aims to deliver a modified PureScript compiler with JavaScript backend replaced with Untyped Plutus Core - a smart contract language of Cardano.

## Links

- Purus development is funded by Cardano Catalyst. [Link to the original proposal](https://cardano.ideascale.com/c/idea/105745).
- Discussions in this repo

## Quick start

An easy way to get started is to use the flake template provided by this project. Here's how to start a new project using the template:

```
mkdir myproject
cd myproject
nix flake init --template github:mlabs-haskell/purus
```

Notice that this requires Nix, for an introduction to the Nix ecosystem, check out [Zero to Nix.](https://zero-to-nix.com/concepts/flakes/), in particular learn more about [Nix flakes](https://zero-to-nix.com/concepts/flakes/).
