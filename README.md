Warp REST
=========

Tutorial on building web applications with Haskell using:

- warp
- acid-state
- scotty
- lens

Testing
=======

```bash
cabal install acid-state
cabal install lens
cabal install scotty
cabal install wai-middleware-static
```

or

```bash
cabal install --only-dependencies
```

```bash
runghc server.hs
```

License
=======

MIT
