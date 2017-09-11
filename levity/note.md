# 言語拡張とGHCオプション
言語拡張 | できること | GHC
------ | ----- | -------
[MagicHash](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#the-magic-hash) | `'x'#`, `3#` などの形式で unboxed value が扱える | 6.8.1
[RankNTypes](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#ghc-flag--XRankNTypes) | 任意のランクの型を許可する | 6.8.1
[PolyKinds](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=polykinds#ghc-flag--XPolyKinds) | カインドポリモーフィック型を許可する | 7.4.1
[DataKinds](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#ghc-flag--XDataKinds) | データ型をカインドに昇格させる | 7.4.1
[TypeInType](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=polykinds#ghc-flag--XTypeInType) | カインド変数を扱える | 8.0.1
[print-explicit-kinds](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using.html?highlight=kind#ghc-flag--fprint-explicit-kinds) | カインドを表示する | 7.8.1
[print-explicit-runtime-reps](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using.html?highlight=kind#ghc-flag--fprint-explicit-runtime-reps) | RuntimeRep を表示する | 8.0.1

## MagicHash
この拡張を有効にすると `x#y = 0` は `x#` という関数が引数 `y` を受け取り `0` を返すという意味になるので注意。

unboxed value | unboxed type
------ | ------
'x'# | Char#
"foo"# | Addr#
3# | Int#
3## | Word#
3.2# | Float#
3.2## | Double#

あんまり意味ないけど、こんな感じで使うことができる。

```haskell
-- MagicHash.hs
{-# LANGUAGE MagicHash #-}
import GHC.Prim ((+#))
import GHC.Types (Int(I#))
import GHC.CString (unpackCString#)

main = do
  putStrLn $ show $ I# (1# +# 5#)
  putStrLn $ show $ unpackCString# "foo"#
```

```bash
$ stack script MagicHash.hs --resolver ghc-8.2.1
Using resolver: ghc-8.2.1 specified on command line
6
"foo"
```

### 参考
- [Primitive Haskell](https://www.fpcomplete.com/blog/2015/02/primitive-haskell)

## RankNTypes
通常では定義できないが、`RankNTypes` を使えば定義できる例。
特に、`forall` だけでなく `context` も対象となる点に注意。

```haskell
>>> f = undefined :: (Eq a => a) -> b  -- context
• Illegal qualified type: Eq a => a
  Perhaps you intended to use RankNTypes or Rank2Types
• In an expression type signature: (Eq a => a) -> b
  In the expression: undefined :: (Eq a => a) -> b
  In an equation for ‘f’: f = undefined :: (Eq a => a) -> b

>>> g = undefined :: (forall a . a) -> b -- forall
  Illegal symbol '.' in type
  Perhaps you intended to use RankNTypes or a similar language
  extension to enable explicit-forall syntax: forall <tvs>. <type>
  
>>> :set -XExplicitForAll
>>> g = undefined :: (forall a . a) -> b -- forall
• Illegal polymorphic type: forall a. a
  Perhaps you intended to use RankNTypes or Rank2Types
• In an expression type signature: (forall a. a) -> b
  In the expression: undefined :: (forall a. a) -> b
  In an equation for ‘g’: g = undefined :: (forall a. a) -> b
```



## PolyKinds

```bash
$ stack repl --resolver ghc-X.Y.Z
$ stack repl --resolver ghc-X.Y.Z --ghci-options "-XPolyKinds"
$ stack repl --resolver ghc-X.Y.Z --ghci-options "-XPolyKinds -fprint-explicit-kinds"
$ stack repl --resolver ghc-X.Y.Z --ghci-options "-XPolyKinds -fprint-explicit-kinds -fprint-explicit-runtime-reps"
```

### データ型に対する動作

```haskell
data T a = MkT
```

GHC-7.10.3 | :type MkT | :kind T | :info T
:------|-------|--------|------
オプション無し | MkT :: T a | T :: * -> * | type role T phantom <br>　data T a = MkT
PolyKinds | MkT :: forall (k :: BOX) (a :: k). T a | T :: k -> * | type role T phantom <br>　data T (a :: k) = MkT
PolyKinds <br> print-explicit-kinds | MkT :: forall (k :: BOX) (a :: k). T k a | T :: k -> * | type role T nominal phantom <br>　data T (k :: BOX) (a :: k) = MkT
PolyKinds <br> print-explicit-kinds <br> print-explicit-runtime-reps | - | - | -

GHC-8.0.2 | :type MkT | :kind T | :info T
:------|-------|--------|------
オプション無し | MkT :: T a | T :: * -> * | type role T phantom <br>　data T a = MkT
PolyKinds | MkT :: forall k (a :: k). T a | T :: k -> * | type role T phantom <br>　data T (a :: k) = MkT
PolyKinds <br> print-explicit-kinds | MkT :: forall k (a :: k). T k a | T :: k -> * | type role T nominal phantom <br>　data T k (a :: k) = MkT
PolyKinds <br> print-explicit-kinds <br> print-explicit-runtime-reps | MkT :: forall k (a :: k). T k a | T :: k -> * | type role T nominal phantom <br>　data T k (a :: k) = MkT

GHC-8.2.1 | :type MkT | :kind T | :info T
:------|-------|--------|------
オプション無し | MkT :: T a | T :: * -> * | type role T phantom <br>　data T a = MkT
PolyKinds | MkT :: forall k (a :: k). T a | T :: k -> * | type role T phantom <br>　data T (a :: k) = MkT
PolyKinds <br> print-explicit-kinds | MkT :: forall k (a :: k). T k a | T :: k -> * | type role T nominal phantom <br>　data T k (a :: k) = MkT
PolyKinds <br> print-explicit-kinds <br> print-explicit-runtime-reps | MkT :: forall k (a :: k). T k a | T :: k -> * | type role T nominal phantom <br>　data T k (a :: k) = MkT

### 関数に対する動作

GHC-7.10.3 | :type ($) | :info ($)
:------|-------|--------
オプション無し | (a -> b) -> a -> b | (a -> b) -> a -> b
PolyKinds | (a -> b) -> a -> b | (a -> b) -> a -> b
PolyKinds <br> print-explicit-kinds | (a -> b) -> a -> b | (a -> b) -> a -> b
PolyKinds <br> print-explicit-kinds <br> print-explicit-runtime-reps | - | - | -

GHC-8.0.2 | :type ($) | :info ($)
:------|-------|--------
オプション無し | (a -> b) -> a -> b | forall (r :: GHC.Types.RuntimeRep) a (b :: TYPE r). <br>　(a -> b) -> a -> b
PolyKinds | (a -> b) -> a -> b | forall (r :: GHC.Types.RuntimeRep) a (b :: TYPE r). <br>　(a -> b) -> a -> b
PolyKinds <br> print-explicit-kinds | (a -> b) -> a -> b | forall (r :: GHC.Types.RuntimeRep) a (b :: TYPE r). <br>　(a -> b) -> a -> b
PolyKinds <br> print-explicit-kinds <br> print-explicit-runtime-reps | (a -> b) -> a -> b | forall (r :: GHC.Types.RuntimeRep) a (b :: TYPE r). <br>　(a -> b) -> a -> b

GHC-8.2.1 | :type ($) | :info ($)
:------|-------|--------
オプション無し | (a -> b) -> a -> b | forall (r :: GHC.Types.RuntimeRep) a (b :: TYPE r). <br>　(a -> b) -> a -> b
PolyKinds | (a -> b) -> a -> b | forall (r :: GHC.Types.RuntimeRep) a (b :: TYPE r). <br>　(a -> b) -> a -> b
PolyKinds <br> print-explicit-kinds | (a -> b) -> a -> b | forall (r :: GHC.Types.RuntimeRep) a (b :: TYPE r). <br>　(a -> b) -> a -> b
PolyKinds <br> print-explicit-kinds <br> print-explicit-runtime-reps | (a -> b) -> a -> b | forall (r :: GHC.Types.RuntimeRep) a (b :: TYPE r). <br>　(a -> b) -> a -> b

## DataKinds

値が型へ、型がカインドへそれぞれ昇格する。

基本的な使い方
```haskell
{-# LANGUAGE DataKinds #-}

data SimpleData a = SimpleData a
```

上記の宣言によって、次のような変化が起きる。

　| 通常 | DataKinds
 ---- | ----- | ------
 値 | SimpleData :: a -> SimpleData a | SimpleData :: a -> SimpleData a
 型 | SimpleData :: \* -> \* <br> | SimpleData :: \* -> \* <br> 'SimpleData :: k -> SimpleData k
 kind | | SimpleData :: \* -> \*
 
また、値が無い場合も定義可能。この場合はコンストラクタが被らないため `'` が省略される。

```haskell
{-# LANGUAGE DataKinds #-}

data SimpleData a = SimpleData a
```

　| 通常 | DataKinds
 ---- | ----- | ------
 値 |  | 
 型 | SimpleData :: \* -> \* | SimpleData :: \* -> \*
 kind | | SimpleData :: \* -> \*
 
[Kind and Type Namespaces](https://ghc.haskell.org/trac/ghc/wiki/GhcKinds/KindsWithoutData#KindandTypeNamespaces) によると、型とカインドは同じ名前空間を共有しているため、実際には `kind` の行は存在しない。

# (un)lifted type と (un)boxed type
 _ | boxed type <br> (ヒープポインタで表現される型) | unboxed type <br> (ヒープポインタで表現されない型)
---- | ---- | -------
lifted type <br> (`bottom` を値に含む型) | Int, Maybe a, Maybe Int# | 存在しない
unlifted type <br> (`bottom` を値に含まない型) | Array# | Int#, Double#

```haskell
-- | Bool  は True, False, bottom のどれかの値になる
--   lifted boxed type
data Bool = False | True
data Maybe a = Just a | Nothing
data Either a b = Left a | Right b
data MyData = MyData Int
data Int = I# Int#

-- | unlifted boxed type
data Array# = ...
data ByteArray# = ...

-- | unlifted unboxed type
data Int# = ...
data Char# = ...
data Float# = ...
data Double# = ...
```

- [GHC-Types](https://www.stackage.org/haddock/lts-9.2/ghc-prim-0.5.0.0/GHC-Types.html)

# カインドと型の推論順序

- [About kind system of Haskell (Part 1)](https://haskell.jp/blog/posts/2017/10-about-kind-system-part1.html) の内容をコピペ

```haskell
-- TestKind.hs

module TestKind where

f :: Maybe -> Int
f _ = 0

g :: Int -> Bool
g '0' = True
g _   = False
```
```bash
$ stack ghc -- -Wall TestKind.hs
[1 of 1] Compiling TestKind         ( TestKind.hs, TestKind.o )

TestKind.hs:5:6: error:
    • Expecting one more argument to ‘Maybe’
      Expected a type, but ‘Maybe’ has kind ‘* -> *’
    • In the type signature:
        f :: Maybe -> Int
```

よって、カインド推論 → 型推論の順で行われる。

# 最初のカインド

基本カインド | 意味
:-------:|---------
\* | lifted boxed types
\# | unlifted types (unlifted boxed type, unlifted unboxed type)
Constraint | 型クラス制約
OpenKind | `*` と `#` のスーパーカインド
Box | カインドのための型。`* :: BOX`, `# :: BOX`, `BOX :: BOX`

## \* と Constraint

```haskell
-- RejectConstraint.hs
f :: Int => Int
f = undefined
```

上記の式は `=>` の左側に期待しているカインドが `Constraint` なので、以下のようなエラーになる。
つまり `(=>) :: Constraint -> * -> *` のような型レベル演算子だと思えば良い。

```bash
$ stack script RejectConstraint.hs --resolver ghc-8.2.1
Using resolver: ghc-8.2.1 specified on command line

/home/bm12/repo/GHC8.2.1-survey/levity/code/RejectConstraint.hs:1:6: error:
    • Expected a constraint, but ‘Int’ has kind ‘*’
    • In the type signature: f :: Int => Int
  |
1 | f :: Int => Int
  |      ^^^
```

しかし、`Core` では両者は同じものとして扱われる。

### `Core` 

```haskell
data Expr b
  = Var   Id
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   b (Expr b)
  | Let   (Bind b) (Expr b)
  | Case  (Expr b) b Type [Alt b]       -- See #case_invariants#
  | Cast  (Expr b) Coercion
  | Tick  (Tickish Id) (Expr b)
  | Type  Type
  | Coercion Coercion
  deriving Data
  
type Type = TYPE 'LiftedRep
```

定義を見る限り、確かに `Type` は全て `TYPE 'LiftedRep` のようである。

以下のサンプルコードを使って、実際に生成される `Core` を確認する。


```haskell
-- AcceptConstraint.hs
f :: Bool
f = True

g :: Eq a => a -> a -> Bool
g a1 a2 = a1 == a2

h :: (Eq a, Eq b) => a -> a -> b -> b -> Bool
h a1 a2 b1 b2 = a1 == a2 && b1 == b2
```

### GHC 7.10.3

```bash
$ stack repl AcceptConstraint.hs --resolver ghc-7.10.3 --ghc-options -ddump-simpl
==================== Tidy Core ====================
f :: Bool
[GblId, Str=DmdType]
f = GHC.Types.True

g :: forall a. Eq a => a -> a -> Bool
[GblId, Arity=3, Caf=NoCafRefs, Str=DmdType]
g = \ (@ a) ($dEq :: Eq a) (a1 :: a) (a2 :: a) -> == @ a $dEq a1 a2

h :: forall a b. (Eq a, Eq b) => a -> a -> b -> b -> Bool
[GblId, Arity=6, Str=DmdType]
h = \ (@ a) (@ b)
      ($dEq :: Eq a) ($dEq1 :: Eq b)
      (a1 :: a) (a2 :: a)
      (b1 :: b) (b2 :: b) -> && (== @ a $dEq a1 a2) (== @ b $dEq1 b1 b2)
```

### GHC 8.0.2

```bash
$ stack repl AcceptConstraint.hs --resolver ghc-8.0.2 --ghc-options -ddump-simpl
==================== Tidy Core ====================
-- RHS size: {terms: 1, types: 0, coercions: 0}
f :: Bool
[GblId, Str=DmdType]
f = GHC.Types.True

-- RHS size: {terms: 8, types: 7, coercions: 0}
g :: forall a. Eq a => a -> a -> Bool
[GblId, Arity=3, Caf=NoCafRefs, Str=DmdType]
g = \ (@ a) ($dEq :: Eq a) (a1 :: a) (a2 :: a) -> == @ a $dEq a1 a2

-- RHS size: {terms: 17, types: 14, coercions: 0}
h :: forall a b. (Eq a, Eq b) => a -> a -> b -> b -> Bool
[GblId, Arity=6, Str=DmdType]
h = \ (@ a) (@ b) 
      ($dEq :: Eq a) ($dEq1 :: Eq b)
      (a1 :: a) (a2 :: a) (b1 :: b) (b2 :: b) -> && (== @ a $dEq a1 a2) (== @ b $dEq1 b1 b2)
```

### GHC 8.2.1

```bash
$ stack repl AcceptConstraint.hs --resolver ghc-8.2.1 --ghc-options -ddump-simpl
==================== Tidy Core ====================
-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
f :: Bool
[GblId]
f = break<4>() GHC.Types.True

-- RHS size: {terms: 8, types: 7, coercions: 0, joins: 0/0}
g :: forall a. Eq a => a -> a -> Bool
[GblId, Arity=3, Caf=NoCafRefs]
g = \ (@ a) ($dEq :: Eq a) (a1 :: a) (a2 :: a) -> break<3>(a1,a2) == @ a $dEq a1 a2

-- RHS size: {terms: 17, types: 14, coercions: 0, joins: 0/0}
h :: forall a b. (Eq a, Eq b) => a -> a -> b -> b -> Bool
[GblId, Arity=6]
h = \ (@ a) (@ b)
      ($dEq :: Eq a) ($dEq1 :: Eq b)
      (a1 :: a) (a2 :: a)
      (b1 :: b) (b2 :: b) -> break<2>(a1,a2,b1,b2) && (break<0>(a1,a2) == @ a $dEq a1 a2) (break<1>(b1,b2) == @ b $dEq1 b1 b2)


```

## tcEqType と eqType

動作の違いを確認するために以下のコードを実行する。
`tcEqType` はカインドが違えば異なるものとして扱うが、`eqType` はカインドが違っていても同じものとして扱う。

```haskell
-- EqType.hs
module Main where

import GHC
import GHC.Paths (libdir)
import HsImpExp (simpleImportDecl)
import MonadUtils (liftIO)
import TcType (tcEqType, toTcType)
import Type (eqType)

main :: IO ()
main =
  runGhc (Just libdir) $ do
    getSessionDynFlags >>= setSessionDynFlags
    setContext [IIDecl $ simpleImportDecl $ mkModuleName "Prelude"]
    t1 <- exprType "True"
    t2 <- exprType "Just True"
    (_, k1) <- typeKind False "Eq Bool"
    (_, k2) <- typeKind False "Maybe Bool"
    -- type equality
    liftIO $ print $ eqType t1 t1 -- True
    liftIO $ print $ eqType t1 t2 -- False
    liftIO $ print $ tcEqType t1 t1 -- True
    liftIO $ print $ tcEqType t1 t2 -- False
    -- kind equality
    liftIO $ print $ eqType k1 k1 -- True
    liftIO $ print $ eqType k1 k2 -- True
    liftIO $ print $ tcEqType k1 k1 -- True
    liftIO $ print $ tcEqType k1 k2 -- False
```

実行結果

```bash
$ stack script EqType.hs --resolver lts-9.3 --package ghc --package ghc-paths
Using resolver: lts-9.3 specified on command line
True
False
True
False
True
True
True
False
```

この結果のうち以下の２行を見れば、動作の違いが明確にわかる。

```haskell
liftIO $ print $ eqType k1 k2 -- True
liftIO $ print $ tcEqType k1 k2 -- False
```

## OpenKind の必要性
- `(->)`　にカインドを与えるために必要。

```haskell
(->) :: * -> * -> *
(->) :: # -> # -> *

-- 両方を扱うために
(->) :: OpenKind -> OpenKind -> *
```

そのため、 `(->)` は引数が全て満たされた状態でしか適用してはいけないという特別なカインド規則があった。

- `error` と `undefined` に型を与えるため

```haskell
error     :: forall (a :: OpenKind). String -> a
undefined :: forall (a :: OpenKind). a)

-- こういう形を両方とも受け入れたい
error Int# "foo" :: Int#
error Int "foo" :: Int

undefined :: Int#
undefined :: Int
```

- 推論時にラムダに束縛された変数にカインドを与えるため

```haskell
\x -> 3# +# x
```

この時まだ `x` の型が lifted なのか unlifted なのかわからないため `OpenKind` を使う。

## OpenKind の問題点

`OpenKind` では `myError s = error ("Blah" ++ s)` のような関数を `Int` と `Int#` の両方に対して動作するようなカインド多相関数として定義することはできない。

```haskell
{-# LANGUAGE MagicHash, ExplicitForAll, PolyKinds #-}

import GHC.Prim (Int#, (+#))
import GHC.Types (Int(I#))

liftedFunc :: Int -> Int -> Int
liftedFunc x y
  | x < y = x + y
  | otherwise = myError "x < y"

unliftedFunc :: Int# -> Int# -> Int#
unliftedFunc x y
  | (I# x) < (I# y) = x +# y
  | otherwise = myError "x < y"

myError :: String -> a
myError s = error ("Error: " ++ s)

main :: IO ()
main = do
  print $ liftedFunc 1 2
  print $ I# (unliftedFunc 1# 2#)
  print $ I# (unliftedFunc 2# 1#) -- error
  print $ liftedFunc 2 1 -- error
```

実行結果

```bash
$ stack script OpenKindProblem2.hs --resolver ghc-7.10.3
Using resolver: ghc-7.10.3 specified on command line

/home/bm12/repo/GHC8.2.1-survey/levity/code/OpenKindProblem2.hs:14:17:
    Couldn't match kind ‘*’ with ‘#’
    When matching types
      t0 :: *
      Int# :: #
    In the expression: myError "x < y"
    In an equation for ‘unliftedFunc’:
        unliftedFunc x y
          | (I# x) < (I# y) = x +# y
          | otherwise = myError "x < y"
```

これが許可されない理由は `myError :: String -> a` の `a` が自動的に `*` カインドとして推論されてしまうからである。

`(->)` のカインドを確認するため、以下の表をまとめた。

 　 | 7.10.3 | 8.0.2 | 8.2.1
----|------|-----|------
:k Int | \* | \* | \*
:k Int# | # | TYPE 'IntRep | TYPE 'IntRep
:k (->) | \* -> \* -> \* | \* -> \* -> \* | TYPE q -> TYPE r -> \*
:k (->) Int | \* -> \* | \* -> \* | \* -> \*
:k (->) Int Int | \* | \* | \*
:k (->) Int# | ERROR | ERROR | \* -> \* 
:k (->) Int# Int# | ERROR | ERROR | \*
:k Int# -> Int# | \* | \* | \*
:k Int -> Int# | \* | \* | \*
:k Int# -> Int | \* | \* | \*

さらにトップレベルに unlifted type の関数を定義した場合は、以下のようなエラーメッセージが返って来る。

```bash
Top-level bindings for unlifted types aren't allowed:
      unliftedValue = 0#
```

`GHC-8.0.2`, `GHC-8.2.1` では `levity polymorphism` を使って `myError` 関数を定義できる。

```haskell
-- OpenKindProblem.hs
{-# LANGUAGE MagicHash, ExplicitForAll, PolyKinds, TypeInType #-}

import GHC.Prim (Int#, (+#), TYPE)
import GHC.Types (Int(I#), RuntimeRep)

liftedFunc :: Int -> Int -> Int
liftedFunc x y
  | x < y = x + y
  | otherwise = myError "x < y"

unliftedFunc :: Int# -> Int# -> Int#
unliftedFunc x y
  | (I# x) < (I# y) = x +# y
  | otherwise = myError "x < y"

myError
  :: forall (l :: RuntimeRep) (a :: TYPE l).
     String -> a
myError s = error ("Error: " ++ s)

main :: IO ()
main = do
  print $ liftedFunc 1 2
  print $ I# (unliftedFunc 1# 2#)
  print $ I# (unliftedFunc 2# 1#) -- error
  print $ liftedFunc 2 1 -- error
```

実行結果
```bash
$ stack script OpenKindProblem.hs --resolver ghc-8.2.1
Using resolver: ghc-8.0.2 specified on command line
3
3
OpenKindProblem.hs: Error: x < y
CallStack (from HasCallStack):
  error, called at /home/bm12/repo/GHC8.2.1-survey/levity/code/OpenKindProblem.hs:19:13 in main:Main
  
$ stack script OpenKindProblem.hs --resolver ghc-8.0.2
Using resolver: ghc-8.0.2 specified on command line
3
3
OpenKindProblem.hs: Error: x < y
CallStack (from HasCallStack):
  error, called at /home/bm12/repo/GHC8.2.1-survey/levity/code/OpenKindProblem.hs:19:13 in main:Main
```

# remedy 後のカインド

基本カインド | 意味 | 備考
:-------:|---------|-----
Constraint | 型クラス制約 | 
Levity | Lifted, Unlifted | 実際の実装では RuntimeRep
TYPE | lifted type, unlifted type | RuntimeRep によって決まる

`Levity` を使う場合

```haskell
data Levity = Lifted | Unlifted

data TYPE (a :: Levity) where
  TYPE :: a -> TYPE Lifted

type * = TYPE Lifted
type # = TYPE Unlifted
type OpenKind = forall (l :: Levity). TYPE l
```

これを使って、既存のカインドを再定義できる。

基本カインド | エイリアス
:-------:|--------
\* | type \* = TYPE Lifted
\#	 | type \# = TYPE Unlifted
OpenKind | type OpenKind = forall (l :: Levity). TYPE l
Box | type Box = TYPE Lifted

```haskell
(->) :: OpenKind -> OpenKind -> *
(->) :: forall (l1 :: Levity) (l2 :: Levity). TYPE l1 -> TYPE l2 -> TYPE Lifted

error :: String -> *
error :: forall (l :: Levity) (a :: TYPE l). String -> a

undefined :: *
undefined :: forall (l :: Levity) (a :: TYPE l). a
```

# kind と type の関係

　| type constructor | data constructor
------ |------ | ------
kind | data TYPE (a :: Levity) <br> data Levity <br> type * = TYPE Lifted <br> type # = TYPE Unlifted | TYPE :: a -> TYPE a <br> Lifted, Unlifted :: Levity <br>　<br>　
type | data Maybe (a :: TYPE Lifted) <br> data Bool <br> type String = [Char] | Just :: a -> Maybe a, Nothing :: Maybe a <br> False, True :: Bool <br>　

つまり、カインドと型は同じ。

# levity polymorphism

以下の形式をサポートしたい。

```haskell
\x -> x :: forall (v :: Levity) (a :: TYPE v). a -> a

myError :: forall (v :: Levity) (a :: TYPE v). String -> a
myError s = error ("Me" ++ s)
```

一般的な levity polymorphism を禁止する代わりに、以下のルールを満たす場合のみ許可する。

- ユーザは型シグネチャによって levity polymorphism を特別にリクエストできる
- Levity-polymorphic 型変数は矢印の右側にのみ出現することができる

```haskell
good :: forall (v :: Levity) (a :: *) (b :: TYPE v). (a -> b) -> a -> b
good g x = g x
```

- `ty:TYPE l` は 矢印の左側には出現できない
- `ty:TYPE l` は 任意の束縛の型の中には出現しない
- コンストラクタのフィールドには出現できない

式 | Lint | 理由
---|-----|-----
(Int -> (ty::TYPE l)) -> Int | OK |
((ty::TYPE l) -> Int) -> Int | NG | `(ty:TYPE l) -> Int` で左側に出現するから
data T l (a::TYPE l) = MkT (Int -> a) | OK | MkT :: (Int -> a) -> T l (a::TYPE l)
MkT Unlifted Int# (\n -> error Unlifted Int# "urk") | OK | MkT :: (Int -> Int#) -> T Unlifted (Int#::TYPE Unlifted)
undefined | OK | undefined :: forall v. forall (a:TYPE v). a

以下のコードは `TYPE` と `levity` がプログラマに公開された時に問題となる。

```haskell
bad :: forall v. forall (a :: TYPE v). a -> a 
bad x = x

good :: forall (a:*). forall v. forall (b :: TYPE v). Show a => a -> b
good x = error (show x) 
```

この問題に対して `levity` 変数が含まないことを制約 `FIXED` として持たせるという解決策もある。

```haskell
(->) :: forall v1,v2, FIXED v1 => TYPE v1 -> TYPE v2 -> *
```

# 実装

以下の定義は問題無い。

```haskell
type family F a where
  F Int# = Int
```

しかし、以下はまずい。

```haskell
type family G a :: k where
  G Int = Int#
  G Bool = Int
```

なぜなら `foo :: G a -> ...` と定義したとき `G a :: TYPE r` となり、 `G a :: TYPE Lifted` か `G a :: TYPE Unlifted` なのか決まらないため、コンパイルができない。

仮に `G a :: TYPE Unlifted` とわかったとしても `unboxed type` によってメモリサイズが異なるため、やはりダメ。

そのため `Levity` の代わりに `RuntimeRep` を利用する。

```haskell
data RuntimeRep = PtrRepLifted | PtrRepUnlifted | IntRep | VoidRep | ...
TYPE :: RuntimeRep -> *
type * = TYPE 'PtrRepLifted
```

この結果、型のカインドから常に値のメモリサイズがわかるようになった。



