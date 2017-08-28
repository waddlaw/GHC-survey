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
#!/usr/bin/env stack
-- stack --resolver lts-9.2 --install-ghc runghc
{-# LANGUAGE MagicHash #-}
import GHC.Prim

main = do
  putStrLn $ show $ I# $ 1# +# 5#
  putStrLn $ show $ unpackCString# "foo"#
```


# (un)lifted type と (un)boxed type
 _ | boxed type <br> (ヒープポインタで表現される型) | unboxed type <br> (ヒープポインタで表現されない型)
---- | ---- | -------
lifted type <br> (`bottom` を値に含む型) | Int, Maybe a, Maybe Int# | 存在しない
unlifted type <br> (`bottom` を値に含まない型) | Array# | Int#, Double#

```haskell
-- | Bool  は True, False, bottom のどれかの値になる
--   lifted boxed type
data Bool = False | True

-- | lifted boxed type
data Int = I# Int#
-- | unlited unboxed type
data Int# = ...
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

