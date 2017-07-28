# Levity Polymorphism (extended version)

# The cost of polymorphism

``` haskell
bTwice :: forall a. Bool -> a -> (a -> a) -> a
bTwice b x f = case b of True  -> f (f x)
                         False -> x
```

- この関数は `polymorphic` in `a`
- `the same function` == the same compiled code for `bTwice` works for any type of argument `x`
- `the colling convention`
  - [呼出規約 - wikipedia](https://ja.wikipedia.org/wiki/%E5%91%BC%E5%87%BA%E8%A6%8F%E7%B4%84)
  - 具体例 (1): `x` が `[a]` (リスト) の場合、ヒープを指し示すレジスタに渡される
  - 具体例 (2): `x` が `Double` 型の場合、`special floating-point register` に渡される
  

`Thus, sharing code conflicts with polymorphism` を解決するための単純で広く用いられる方法は、全ての値を `ヒープに格納された値を指すポインタ` として表現すること。 (`sharing code conflicts` がよくわかっていない)

- この手法の問題点は `速度が非常に遅い`。

たいていの `polymorphic language` では `unboxed values` の形式がサポートされている。

- `unboxed value` とは、ポインタとしての表現ではなく、値そのもの

`Levity polymorphism` は `GHC 8.0.1` から実装されている。`Cyclone` という言語も同様のアプローチをとっている。

- [Cyclone is a safe dialect of C.](https://cyclone.thelanguage.org/)
- [Cyclone - wikipedia](https://en.wikipedia.org/wiki/Cyclone_(programming_language))
