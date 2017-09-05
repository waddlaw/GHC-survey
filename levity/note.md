# 言語拡張
言語拡張 | できること
------ | -----
[MagicHash](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#the-magic-hash) | `'x'#`, `3#` などの形式で unboxed value が扱える

# MagicHash
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

## 参考
- [Primitive Haskell](https://www.fpcomplete.com/blog/2015/02/primitive-haskell)


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

# カインドの種類

## 最初

基本カインド | 意味
:-------:|---------
\* | lifted boxed types
\# | unlifted types (unlifted boxed type, unlifted unboxed type)
Constraint | 型クラス制約
OpenKind | `*` と `#` のスーパーカインド

### \* と Constraint

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

#### `Core` 

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

#### GHC 7.10.3

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

#### GHC 8.0.2

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

#### GHC 8.2.1

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

### tcEqType と eqType

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

```
liftIO $ print $ eqType k1 k2 -- True
liftIO $ print $ tcEqType k1 k2 -- False
```
