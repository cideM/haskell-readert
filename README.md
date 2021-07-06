# Haskell ReaderT Application Architecture Example

TODO: Document

```shell
$ nix-shell --packages cabal2nix --run 'cabal2nix . > project.nix'
$ cabal v2-run exe:haskell-readert
$ cabal v2-run test:tests
$ ghcid --no-height-limit --clear --reverse --target=exe:haskell-readert
$ ghcid --no-height-limit --clear --reverse --target=test:tests
```
