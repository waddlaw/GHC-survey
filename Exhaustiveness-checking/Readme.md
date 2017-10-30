# Exhaustiveness Checking

以下の2つについて `Exhaustiveness Checking` を行うようになった。
- `EmptyCase`
- `pattern synonyms`

## EmptyCase

```haskell
-- EmptyCase.hs
{-# LANGUAGE EmptyCase #-}
module Test where

test :: Bool -> Int
test a = case a of
```

`GHC-8.0.2` で読み込んだ場合は何も警告されない。

```haskell
$ stack repl --resolver=ghc-8.0.2 --ghc-options="-Wall" EmptyCase.hs
Attempting to load anyway.
Configuring GHCi with the following packages:
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Test             ( /home/bm12/repo/guchi/GHC8.2.1-survey/Exhaustiveness-checking/src/EmptyCase.hs, interpreted )
Ok, modules loaded: Test.
Loaded GHCi configuration from /tmp/ghci11195/ghci-script
*Test>
```

`GHC-8.2.1` で読み込んだ場合は、以下のように警告が出るようになった。

```haskell
$ stack repl --resolver=ghc-8.2.1 --ghc-options="-Wall" EmptyCase.hs
Attempting to load anyway.
Configuring GHCi with the following packages:
GHCi, version 8.2.1: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Test             ( /home/bm12/repo/guchi/GHC8.2.1-survey/Exhaustiveness-checking/src/EmptyCase.hs, interpreted )

/home/bm12/repo/guchi/GHC8.2.1-survey/Exhaustiveness-checking/src/EmptyCase.hs:5:10: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In a case alternative:
        Patterns not matched:
            False
            True
  |
5 | test a = case a of
  |          ^^^^
Ok, 1 module loaded.
Loaded GHCi configuration from /tmp/ghci11271/ghci-script
*Test>
```

実行するとどうなるか？

```haskell
*Test> test True
*** Exception: /home/bm12/repo/guchi/GHC8.2.1-survey/Exhaustiveness-checking/src/EmptyCase.hs:5:10-13: Non-exhaustive patterns in case

*Test> test False
*** Exception: /home/bm12/repo/guchi/GHC8.2.1-survey/Exhaustiveness-checking/src/EmptyCase.hs:5:10-13: Non-exhaustive patterns in case
```

そもそも `EmptyCase` とか邪悪な感じしかしないんだけど、これが役立つ時って何があるんだろう？

## 参考
### ghc trac
- [Exhaustiveness check for EmptyCase (Trac #10746)](https://phabricator.haskell.org/D2105)
- [No non-exhaustive pattern match warning given for empty case analysis](https://ghc.haskell.org/trac/ghc/ticket/10746)
- [Exhaustiveness checks for pattern synonyms](https://ghc.haskell.org/trac/ghc/ticket/8779)
