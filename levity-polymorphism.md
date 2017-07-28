# Levity Polymorphism (extended version)

* [paper](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/levity-1.pdf)

# 1. The cost of polymorphism

``` haskell
bTwice :: forall a. Bool -> a -> (a -> a) -> a
bTwice b x f = case b of True  -> f (f x)
                         False -> x
```

- この関数は `polymorphic` in `a`
  - 今後出てくる `polymorphic` は全て `parametric polymorphism` のこと。それとは別に `ad-hoc polymorphism` などがある。(これは `Haskell` では主に型クラス等を使って実現)
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

# 2. Background: performance through unboxed types

## 2.1 Unboxed values

1~n までの整数の合計を計算する以下のループを考える。

```haskell
sumTo :: Int -> Int -> Int
sumTo acc 0 = acc
sumTo acc n = sumTo (acc + n) (n - 1)
```

`GHC` は `Int` 型の値を `2ワードのセル` へのポインタとして表現する。

- 最初のワードは `descriptor`
- 次のワードが `Int` の実際の値

`Int` を使って `sumTo` を計算すると、驚くほど遅い。

- 基底部では `sumTo` の第二引数を評価し、`ポインタをたどって` 値を取得し、0かどうか確認する必要がある。
- 再帰部では、`(acc + n)` と `(n - 1)` のための `サンクを確保` し、再びそれを繰り返す。

これに対して、`C` コンパイラは `a three-machine-instruction loop` を利用するため、メモリへのアクセスは全く発生しない。
これが、パフォーマンスに莫大な差を与える。

そのため、`GHC` では `unboxed integer` を表す `Int#` 型が組み込みで提供されている。
`Int#` はポインタによる表現ではなく、整数値自身である。

`unboxed integer` を使った `sumTo` の実装は次のようになる。

```haskell
sumTo# :: Int# -> Int# -> Int#
sumTo# acc 0# = acc
sumTo# acc n = sumTo# (acc +# n) (n -# 1#)
```

見た目上のコードは良く似ていても、コンパイルされるコードは大きく異なり、`C` で手書きしたものと、本質的に同等のものが生成される。

`GHC` の正格性解析機とその他の最適化機構により、`sumTo` は `sumTo#` に変換されることがよくある。

また、`Int#` 以外にも `Char#`, `Double#` などの `unboxed type` や、その型のためのプリミティブ演算子が同様に用意されている。

これらの `unboxed values` を使って、`Haskell` の `boxed version` が定義される。

```haskell
data Int = I# Int#

plusInt :: Int -> Int -> Int
plusInt (I# i1) (I# i2) = I# (i1 +# i2)
```

ここで、 `Int` はデータコンストラクタ `I#` と `Int#` 型のフィールドを1つ持つ、通常の代数的データ型であり、特別なことは何もない。
`plusInt` 関数は単純に自身の引数でパターンマッチを行い、それらにマッチしたコンテンツ (`Int#` 型の値 `i1`, `i2`) を `(+#)` を使って足しあわせ、`I#` で結果を `box` にする。

------

- [9.2. Unboxed types and primitive operations](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#unboxed-types-and-primitive-operations)
- [GHC.Prim](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-prim-0.5.1.0/GHC-Prim.html)

## 2.2 Boxed vs. unboxed and lifted vx. unlifted

