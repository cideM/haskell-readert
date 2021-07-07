# Haskell ReaderT Application Architecture Example

This repository demonstrates how to use the `ReaderT` pattern to structure your Haskell code.

The goal is to have a body of code that tries to be minimal but also not too far away from production-reader at the same time. For example, I kept everything in a single library file, which is unlikely in production but helps with having the entire thing in front of you. On the other hand imports are qualified and I'm using `Text` instead of `String`, to satisfy typical production needs.

The example is heavily inspired by the documentation for [RIO](https://hackage.haskell.org/package/rio), particularly the point about

> a perfect balance point in the composability/concreteness space

You might notice that I'm essentially reimplementing the environment type in my tests. The reason for this is that I expect a production environment to include many things which are irrelevant for more focused integration or even unit tests. In such cases you have two choices. You can share a single environment, at the cost of having to supply fake implementations for all the things you don't care about. Or you implement subsets of the environment, but then you need to reimplement some of the `Has` typeclasses and potentially other concerns, such as Katip logging in this example.

I'm sure there are more advanced solutions to this problem, but `ReaderT` is generally appreciated for not being too fancy, and as such I kept the repository simple as well.

Feel free to suggest changes through issues or pull requests!

## Commands

1. Generate `project.nix` if no Nix environment is available or Nix compilation failed.
2. Run the main executable
3. Run the tests
4. Start `ghcid` for checking the executable
4. Start `ghcid` for checking the tests

```shell
$ nix-shell --packages cabal2nix --run 'cabal2nix . > project.nix'
$ cabal v2-run exe:haskell-readert
$ cabal v2-run test:tests
$ ghcid --no-height-limit --clear --reverse --target=exe:haskell-readert
$ ghcid --no-height-limit --clear --reverse --target=test:tests
```
