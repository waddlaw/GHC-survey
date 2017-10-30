# Exhaustiveness Checking

以下の2つについて `Exhaustiveness Checking` を行うようになった。
- `EmptyCase`
- `pattern synonyms`

## EmptyCase

`EmptyCase` 拡張は `case` の本体が無い記法。

### 問題のコード

```haskell
-- EmptyCase.hs
{-# LANGUAGE EmptyCase #-}
module Test where

test :: Bool -> Int
test a = case a of
```

### GHC-8.0.2

```bash
$ stack repl --resolver=ghc-8.0.2 --ghc-options="-Wall" EmptyCase.hs
Attempting to load anyway.
Configuring GHCi with the following packages:
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Test             ( /home/bm12/repo/guchi/GHC8.2.1-survey/Exhaustiveness-checking/src/EmptyCase.hs, interpreted )
Ok, modules loaded: Test.
Loaded GHCi configuration from /tmp/ghci11195/ghci-script
*Test>
```

### GHC-8.2.1

```bash
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

### 実行するとどうなるか？

```bash
*Test> test True
*** Exception: /home/bm12/repo/guchi/GHC8.2.1-survey/Exhaustiveness-checking/src/EmptyCase.hs:5:10-13: Non-exhaustive patterns in case

*Test> test False
*** Exception: /home/bm12/repo/guchi/GHC8.2.1-survey/Exhaustiveness-checking/src/EmptyCase.hs:5:10-13: Non-exhaustive patterns in case
```

そもそも `EmptyCase` とか邪悪な感じしかしないんだけど、これが役立つ時って何があるんだろう？

## pattern synonyms

`PatternSynonyms` 拡張は `type` キーワードと対をなす存在だと思えば理解が早い。

キーワード | 意味
----|-----
type | **タイプ** コンストラクタのエイリアス
pattern | **データ** コンストラクタのエイリアス

仮にこんな感じで `Bool` 型と同じ振る舞いになって欲しいとしよう。同じ振る舞いというのは **パターンマッチ** (型クラスのインスタンス) が同一ということ。

```haskell
-- このコードは当然コンパイルできない
data Answer = Yes | No

showAnswer :: Answer -> String
showAnswer ans = if ans then "YEEEEES" else "NOOOOOO"
```

こういう時に `PatternSynonyms` を使うことができる。

```haskell
{-# LANGUAGE PatternSynonyms #-}

type Answer = Bool

pattern Yes :: Answer
pattern Yes = True

pattern No :: Answer
pattern No = False

showAnswer :: Answer -> String
showAnswer ans = if ans then "YEEEEES" else "NOOOOOO"
```

実行してみる。

```bash
>>> showAnswer No
"NOOOOOO"
>>> showAnswer Yes
"YEEEEES"
```

このような使い方を `bidirectional synonyms` と読んでいる。
 - 左から右のパターンマッチが可能
 - 右から左にパターンマッチが可能
 - つまり、データコンストラクタのエイリアスを定義していることと同じ


それに対して `Uni-directional (pattern-only) synonyms` というのもある。

これは本当に、パターンマッチに好きな名前を付けて利用できるというイメージで良いと思う。

```haskell
{-# LANGUAGE PatternSynonyms #-}

pattern Head :: a -> [a]
pattern Head x <- x:_ -- _ を含む物は = では定義できない

showHead :: [a] -> String
showHead (Head _) = undefined
showHead _  = "No head!"
```

実行するとこんな感じ。

```bash
>>> showHead "a"
"*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at /home/bm12/repo/guchi/GHC8.2.1-survey/Exhaustiveness-checking/src/PatternSynonyms-example.hs:20:21 in main:Main

>>> showHead ""
"No head!"
```

`pattern-only synonyms` 記法は以下の4種類がある。

```
pattern conid varid1 ... varidn ~ pat
pattern conid varid1 ... varidn := pat
pattern conid varid1 ... varidn -> pat
pattern conid varid1 ... varidn <- pat
```

## 問題のコード

```haskell
{-# LANGUAGE PatternSynonyms #-}
module Patterns where

data ErrorCall = ErrorCallWithLocation String String deriving (Eq, Ord)

pattern ErrorCall :: String -> ErrorCall
pattern ErrorCall err <- ErrorCallWithLocation err _
  where ErrorCall err =  ErrorCallWithLocation err ""

getMsg :: ErrorCall -> String
getMsg (ErrorCall y) = y

getMsg' :: ErrorCall -> String
getMsg' (ErrorCallWithLocation y _) = y
```

### GHC-8.0.2

```bash
$ stack repl --resolver=ghc-8.0.2 --ghc-options="-Wall" PatternSynonyms.hs

Warning: Couldn't find a component for file target /home/bm12/repo/guchi/GHC8.2.1-survey/Exhaustiveness-checking/src/PatternSynonyms.hs. Attempting to load anyway.
Configuring GHCi with the following packages:
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Patterns         ( /home/bm12/repo/guchi/GHC8.2.1-survey/Exhaustiveness-checking/src/PatternSynonyms.hs, interpreted )

/home/bm12/repo/guchi/GHC8.2.1-survey/Exhaustiveness-checking/src/PatternSynonyms.hs:11:1: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘getMsg’: Patterns not matched: _
Ok, modules loaded: Patterns.
Loaded GHCi configuration from /tmp/ghci13076/ghci-script
*Patterns>
```

### GHC-8.2.1

```bash
$ stack repl --resolver=ghc-8.2.1 --ghc-options="-Wall" PatternSynonyms.hs

Warning: Couldn't find a component for file target /home/bm12/repo/guchi/GHC8.2.1-survey/Exhaustiveness-checking/src/PatternSynonyms.hs. Attempting to load anyway.
Configuring GHCi with the following packages:
GHCi, version 8.2.1: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Patterns         ( /home/bm12/repo/guchi/GHC8.2.1-survey/Exhaustiveness-checking/src/PatternSynonyms.hs, interpreted )

/home/bm12/repo/guchi/GHC8.2.1-survey/Exhaustiveness-checking/src/PatternSynonyms.hs:11:1: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘getMsg’: Patterns not matched: _
   |
11 | getMsg (ErrorCall y) = y
   | ^^^^^^^^^^^^^^^^^^^^^^^^
Ok, 1 module loaded.
Loaded GHCi configuration from /tmp/ghci13196/ghci-script
*Patterns>
```

## 参考
### ghc trac
- [Exhaustiveness check for EmptyCase (Trac #10746)](https://phabricator.haskell.org/D2105)
- [No non-exhaustive pattern match warning given for empty case analysis](https://ghc.haskell.org/trac/ghc/ticket/10746)
- [Exhaustiveness checks for pattern synonyms](https://ghc.haskell.org/trac/ghc/ticket/8779)
- [Pattern Synonyms](https://ghc.haskell.org/trac/ghc/wiki/PatternSynonyms)
- [Pattern Synonymsで遊ぶ](https://qiita.com/as_capabl/items/d2eb781478e26411a44c)
