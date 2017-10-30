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
[1 of 1] Compiling Test
*Test>
```

### GHC-8.2.1

```bash
$ stack repl --resolver=ghc-8.2.1 --ghc-options="-Wall" EmptyCase.hs
[1 of 1] Compiling Test

EmptyCase.hs:5:10: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In a case alternative:
        Patterns not matched:
            False
            True
  |
5 | test a = case a of
  |          ^^^^
*Test>
```

### 実行するとどうなるか？

```bash
*Test> test True
*** Exception: ...

*Test> test False
*** Exception: ...
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

pattern P :: ()
pattern P = ()

foo :: () -> ()
foo P = ()
```

### GHC-8.0.2

```bash
$ stack repl --resolver=ghc-8.0.2 --ghc-options="-Wall" PS2.hs
[1 of 1] Compiling Main

PS2.hs:7:1: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘foo’: Patterns not matched: _
*Main>
```

### GHC-8.2.1

```bash
$ stack repl --resolver=ghc-8.2.1 --ghc-options="-Wall" PS2.hs
[1 of 1] Compiling Main

PS2.hs:7:1: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘foo’: Patterns not matched: _
  |
7 | foo P = ()
  | ^^^^^^^^^^
*Main>
```

新たに導入されたプラグマを使うことで、このエラーを抑制できる。

```haskell
{-# LANGUAGE PatternSynonyms #-}

pattern P :: ()
pattern P = ()
{-# COMPLETE P #-}

foo :: () -> ()
foo P = ()
```

## 参考
### ghc trac
- [Exhaustiveness check for EmptyCase (Trac #10746)](https://phabricator.haskell.org/D2105)
- [No non-exhaustive pattern match warning given for empty case analysis](https://ghc.haskell.org/trac/ghc/ticket/10746)
- [Exhaustiveness checks for pattern synonyms](https://ghc.haskell.org/trac/ghc/ticket/8779)
- [Pattern Synonyms](https://ghc.haskell.org/trac/ghc/wiki/PatternSynonyms)
- [Pattern Synonymsで遊ぶ](https://qiita.com/as_capabl/items/d2eb781478e26411a44c)
