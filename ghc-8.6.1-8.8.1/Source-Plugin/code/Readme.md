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

### add-decl

```shell
$ cabal new-repl add-decl
[g = 1 + 1, data MyData = MyData, a = (), $(return []), b :: Int,
 b = g + 2]
```

### count-strict-fields

```shell
$ cabal new-repl use-count-strict-fields
...
========================================
StrictData : True
strict fields: 8/8
========================================

[2 of 2] Compiling Lib              ( Lib.hs, interpreted )

========================================
StrictData : False
strict fields: 2/3
========================================
```

### use-replace-prelude

```shell
$ cabal new-repl use-replace-prelude
...
[1 of 2] Compiling Lib              ( Lib.hs, interpreted )
import modules:
[import RIO]
[2 of 2] Compiling Example          ( Example.hs, interpreted )
import modules:
[import RIO, import Lib]
```

### everywhere-has-call-stack

```shell
$ cabal new-repl use-trace
```