# Readme

```shell
$ ghc A.hs -package ghc
```

- [Providing type checker plugin on command line results in false cyclic import error](https://ghc.haskell.org/trac/ghc/ticket/10077)
- [Type Checker Plugins](https://ghc.haskell.org/trac/ghc/wiki/Plugins/TypeChecker#FAQ)

## 実行結果

```shell
$ cabal new-build all
```

### basic

```shell
$ cabal new-repl basic
parsePlugin:
module Example where
a = ()
$(return [])
typeCheckPlugin (rn): a = ()
interface loaded: Language.Haskell.TH.Lib.Internal
meta: return []
typeCheckPlugin (rn):
typeCheckPlugin (rn):
Nothing
typeCheckPlugin (tc):
{$trModule
   = Module (TrNameS "basic-0.1.0.0-inplace"#) (TrNameS "Example"#),
 a = ()}
```

### basic-simple

```shell
$ cabal new-repl basic-simple
parsePlugin:
module Example where
a = ()
$(return [])
typeCheckPlugin (rn): a = ()
interface loaded: Language.Haskell.TH.Lib.Internal
interface loaded: Language.Haskell.TH.Syntax
meta: return []
typeCheckPlugin (rn):
typeCheckPlugin (rn):
Nothing
typeCheckPlugin (tc):
{$trModule
   = Module
       (TrNameS "basic-simple-0.1.0.0-inplace"#) (TrNameS "Example"#),
 a = ()}
```

### print-type-info

```shell
$ cabal new-repl print-type-info
```