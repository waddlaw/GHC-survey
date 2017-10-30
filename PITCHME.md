
<span class="menu-title" style="display: none">Exhaustiveness Checking</span>


# Exhaustiveness Checking

---

exhaustive を発音してみよう！

> \ig-ˌzȯ-ˈsti-və-tē\

---

<span class="menu-title" style="display: none">Exhaustiveness Checking とは？</span>


## Exhaustiveness Checking <br>とは 何か？

+++

Question: この関数は何が `BAD` でしょうか？
<br>

```haskell
bad :: Bool -> Bool
bad True = undefined
```

+++

Answer: 

```haskell
good :: Bool -> Bool
good True = undefined
good False = undefined
```

+++

実行してみましょう！

```
$ stack repl --resolver=ghc-8.0.2 bad.hs
```

```
$ stack repl --resolver=ghc-8.0.2 bad.hs
[1 of 1] Compiling Main
Ok, 1 module loaded.
*Main>
```

+++

適当な事言わないでくださいよ・・・<br>
警告出てないじゃないですか！

+++

Question: なぜ警告が出ないのでしょうか？ 

+++

Answer: 

デフォルトでは警告は出ません！

ちゃんと `-Wall` オプションを使いましょう。

+++

`-Wall は -Warning all` の略です。

```
$ stack repl --resolver=ghc-8.0.2 --ghc-options="-Wall" Bad.hs
[1 of 1] Compiling Main
Bad.hs:2:1: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘bad’: Patterns not matched: False
Ok, modules loaded: Main.
*Main>
```

+++

ちなみに `ghc-8.2.1` ではこうなります。

```
$ stack repl --resolver=ghc-8.2.1 --ghc-options="-Wall" Bad.hs
[1 of 1] Compiling Main

Bad.hs:2:1: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘bad’: Patterns not matched: False
  |
2 | bad True = undefined
  | ^^^^^^^^^^^^^^^^^^^^
Ok, 1 module loaded.
*Main>
```

---

網羅していないパターンマッチを警告してくれる

```haskell
-- Bad.hs
bad :: Bool -> Bool
bad True = undefined
```

```
$ stack repl --resolver=ghc-8.2.1 --ghc-options="-Wall" Bad.hs
Bad.hs:2:1: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘bad’: Patterns not matched: False
  |
2 | bad True = undefined
  | ^^^^^^^^^^^^^^^^^^^^
Ok, 1 module loaded.
```

---

どんなチェックが追加されたか？

- EmptyCase 言語拡張
- PatternSynonyms 言語拡張

---

EmptyCase 言語拡張

+++

EmptyCase の具体例

```haskell
-- EmptyCase.hs
{-# LANGUAGE EmptyCase #-}
module Test where

test :: Bool -> Int
test a = case a of
```

<br>

Question: どういう時に使えるんですか？

+++

> This is most useful when you have a type that you know for sure has no values, but Haskell‘s syntax and type system force you to do something with a hypothetical such value anyway. 

Site a [Basic Syntax Extensions](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions#emptycase)

+++

> これは、値がないことがわかっている型を持っているときに最も役に立ちます。しかし、Haskellの構文と型システムは、仮にそのような値で何かをやらなければなりません。

By Google 翻訳

---

とりあえず、実行してみましょう。

```haskell
-- EmptyCase.hs
{-# LANGUAGE EmptyCase #-}
module Test where

test :: Bool -> Int
test a = case a of
```

+++

```
$ stack repl --resolver=ghc-8.0.2 --ghc-options="-Wall" EmptyCase.hs
[1 of 1] Compiling Test
Ok, modules loaded: Test.
*Test>
```

```bash
$ stack repl --resolver=ghc-8.2.1 --ghc-options="-Wall" EmptyCase.hs
[1 of 1] Compiling Test

/Users/bm12/Desktop/EmptyCase.hs:5:10: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In a case alternative:
        Patterns not matched:
            False
            True
  |
5 | test a = case a of
  |          ^^^^
Ok, 1 module loaded.
*Test>
```

+++

# 完...

+++

そもそも `case` の本体いらないから `EmptyCase` 使ってるのに、警告出されてもって感じするけど

人間は忘れちゃうからね

---


PatternSynonyms 言語拡張

+++

とりあえずの理解

> 値コンストラクタのエイリアスを<br>定義するための方法

+++

こんなの一般常識レベルの話だけど、<br>型コンストラクタのエイリアスを<br>定義する方法は知ってるよね？

+++

Question:

型コンストラクタのエイリアスを<br>定義するためのキーワードは何？

+++

Special Hint !!!!

1. `data`
1. `newtype`
1. `class`
1. `instance`

+++

ヒントに騙されずに自分を信じて<br>`type` と答えましょうね。

+++

Question: おかしいところはどこ？

```haskell
-- type constructor alias
type Answer = Bool

-- data constructor alias
data Yes = True
data No  = False
```

+++

Answer: 

```haskell
-- type constructor alias
type Answer = Bool

-- data constructor alias (どう見てもここが頭おかしい)
data Yes = True
data No  = False
```

+++

`data` はもう使われてるから使えないんだよ！

↓

じゃあ `pattern` でいいや！

<br>

(言いたいことはわかる・・・)

---

PatternSynonyms (bidirectional synonyms) の例

```haskell
{-# LANGUAGE PatternSynonyms #-}
type Answer = Bool

-- data constructor alias
pattern Yes = True
pattern No  = False
```

```
$ stack repl --resolver=ghc-8.2.1 --ghc-options="-Wall" PatternSynonym.hs
*Main> :t Yes
Yes :: Bool
*Main> Yes == True
True
*Main> Yes == No
False
```

+++

どうでも良いですが `Synonyms` という単語をみるとあの人の顔しか出てきません。


+++

そう、みんなのアイドル！

Haskeller の憧れ！

マイケル！

---

前のスライドで、さらっと `bidirectional synonyms` とカッコで記述したの気づきました？

+++

Question:

bidirectional は日本語でどういう意味でしょう？

+++

# 双

By Bing 翻訳

+++

すみません・・・。双方向です。

(Bing が嫌いなわけじゃないよ)

---

パターンマッチの復習

<br>

値を構築するパターンマッチ

```haskell
toIntMaybe :: Int -> Maybe Int
toIntMaybe 0 = Nothing
toIntMaybe x = Just x
```

値を分解するパターンマッチ

```haskell
fromIntMaybe :: Maybe Int -> Int
fromIntMaybe Nothing = 0
fromIntMaybe (Just x) = x
```

+++

Question: 

`toIntMaybe . fromIntMaybe = ?`

`fromIntMaybe . toIntMaybe = ?`

<br>

Hint: `id` ではないです。

+++

Answer: 大人を信じるのはやめよう！

+++

True Answer:

`toIntMaybe . fromIntMaybe = id`

`fromIntMaybe . toIntMaybe = id`

---

つまり、値コンストラクタって<br>双方向のパターンマッチができるんですよ！

+++

そう考えると `pattern` ってキーワードになってる意味がわかってきますよね？

```haskell
{-# LANGUAGE PatternSynonyms #-}
type Answer = Bool

-- data constructor alias
pattern Yes = True
pattern No  = False
```

---

`Bool` のパターンマッチを再利用してみましょう。

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

+++

Question: 以下のコードを実行すると何が表示されるでしょう？

```
*Main> showAnswer True
???
```

+++

Answer:

```
*Main> showAnswer True
"YEEEEEES"
```

+++

よく見て。

さっき信じたらダメだって言ったのに・・・。

+++

True Answer:

```
*Main> showAnswer True
"YEEEEES"
```

---

Uni-directional (pattern-only) synonyms もある。

```
pattern conid varid1 ... varidn ~ pat
pattern conid varid1 ... varidn := pat
pattern conid varid1 ... varidn -> pat
pattern conid varid1 ... varidn <- pat
```

wiki には上記4種類の記法が載っていた。

+++

最初に言っていたことを覚えているだろうか。

<br>

PatternSynonyms 拡張は値コンストラクタのエイリアスを定義するためにあります。

<br>

しかし、値コンストラクタも実は (双方向の) パターンマッチに過ぎないということが徐々にわかってきましたね。

+++

そう、名前通り実は・・・

PatternSynonyms 拡張は「パターン」のエイリアスを定義するためにあります。

---

これは単方向 (値の分解) のパターンマッチ `x <- x:_` に `Head` という別名を付けている例です。

```haskell
{-# LANGUAGE PatternSynonyms #-}

pattern Head :: a -> [a]
pattern Head x <- x:_

showHead :: [a] -> String
showHead (Head _) = undefined
showHead _  = "No head!"
```

ちょっとわかりにくいので、いつものパターンマッチも載せておくね

```haskell
showHead :: [a] -> String
showHead (x:_) = undefined
showHead _  = "No head!"
```

+++

ぶっちゃけ使った事ないからわからないけど、実際のパターンマッチを隠しておくことで、構造を変更した際の変更箇所を pattern に集約できるという利点はありそう。

---

構造の変化を pattern の Head で吸収している例

```haskell
{-# LANGUAGE PatternSynonyms #-}

type Container a = [a]

pattern Head :: a -> Container a
pattern Head x <- x:_

showHead :: Container a -> String
showHead (Head _) = undefined
showHead _  = "No head!"
```

```haskell
{-# LANGUAGE PatternSynonyms #-}

type Container a = (a, a)

pattern Head :: a -> Container a
pattern Head x <- (x,_)

showHead :: Container -> String
showHead (Head _) = undefined
showHead _  = "No head!"
```

---

PatternSynonyms 拡張は意外と奥が深いし、使いようによっては値の変更に強い構造を作れると思うので、気になった人は各自調べてください。

---

# Thanks!
