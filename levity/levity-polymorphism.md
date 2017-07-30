# Levity Polymorphism (extended version)

- [paper](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/levity-1.pdf)
- [NoSubKinds](https://ghc.haskell.org/trac/ghc/wiki/NoSubKinds)
    - [日本語訳](https://github.com/waddlaw/GHC8.2.1-survey/blob/master/levity/NoSubKinds.md)
- [What is Levity polymorphism](https://stackoverflow.com/questions/35318562/what-is-levity-polymorphism/35320729#35320729)
- [The data type Type and its friends](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/TypeType)
- [Levity polymorphism](https://ghc.haskell.org/trac/ghc/wiki/LevityPolymorphism)

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
本論文が取り組むパフォーマンスへの挑戦を最初に記述する。我々はこれからの議論を具体的にするため `Haskell`<sup>2</sup> と `GHC` コンパイラを用いるが、多くの事柄が、他の多相的な言語にも同様に適用できる。他の言語とコンパイラについては8章で議論する。

-----
2. `GHC` はハイパフォーマンスコードをサポートするために、様々な方法で `Haskell` を拡張している。そのため、`Haskell` と記述されている部分は `GHC Haskell` の略だと思って欲しい。

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

`Int` を使って `sumTo` を計算すると、驚くほど遅い。それぞれの繰り返しが第二引数<sup>3</sup>を評価しようとするためだ。

- 基底部では `sumTo` の第二引数を評価し、`ポインタをたどって` 値を取得し、0かどうか確認する必要がある。
- 再帰部では、`(acc + n)` と `(n - 1)` のための `サンクを確保` し、再びそれを繰り返す。

これに対して、`C` コンパイラは `a three-machine-instruction loop` を利用するため、メモリへのアクセスは全く発生しない。
これが、パフォーマンスに莫大な差を与える。

そのため、`GHC` では `unboxed integer` を表す `Int#` 型が組み込みで提供されている[12]。
`Int#` はポインタによる表現ではなく、整数値自身である。

`unboxed integer` を使った `sumTo` の実装は次のようになる。<sup>4</sup>

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

ここで、 `Int` はデータコンストラクタ `I#` と、単一の `Int#` 型のフィールドを持つ、通常の代数的データ型であり、特別なことは何もない。
`plusInt` 関数は単純に自身の引数でパターンマッチを行い、それらにマッチしたコンテンツ (`Int#` 型の値 `i1`, `i2`) を `(+#)` を使って足しあわせ、`I#` で結果を `box` にする。

------

3. 思い出して欲しい、`Haskell` は遅延言語である。そのため、第二引数は必要となるまで評価されない。
4. 接尾辞 `#` はコンパイラによって一切特別な処理が加えられることはない。これは、 `unboxed` な値であることを読者に示唆するための素朴な命名規則である。

- [9.2. Unboxed types and primitive operations](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#unboxed-types-and-primitive-operations)
- [GHC.Prim](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-prim-0.5.1.0/GHC-Prim.html)

## 2.2 Boxed vs. unboxed and lifted vs. unlifted
一般的には、`boxed value` はヒープへのポインタとして表されるが、その一方で `unboxed value` は値自身である。それゆえ、`unboxed value` はサンクになることができず、 `unboxed` 型の引数は必ず値として渡されることとなる。

`Haskell` もまた `levity` を検討する必要がある。これは、`lifted` と `unlifted` の選択を意味する。`lifted type` は `lazy` である。これは、`lifted` だと思われる。なぜなら、`lifted` ヒープに1つ余分な要素が1つあり、これが非決定性計算を表現しているからである。例えば、`Haskell` の `Bool` 型は `lifted` であり、`True`, `False`, `⊥` のうちのどれかである。それに対して `unlifted type` は `strict` である。そのため、`⊥` は `unlifted type` には存在しない。

なぜなら、 `Haskell` は `lifted type` は `boxed` にならなければならないという制約を、実行時にサンクとすることで、遅延評価を表現しているからである。



--------

- [9.2.1. Unboxed types](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#unboxed-types)

> Note: a “boxed” type means that a value is represented by a pointer to a heap object; a “lifted” type means that terms of that type may be bottom.

> a primitive value might be represented by a pointer to a heap-allocated object. Examples include Array#, the type of primitive arrays. Thus, Array# is an unlifted, boxed type. A primitive array is heap-allocated because it is too big a value to fit in a register, and would be too expensive to copy around; in a sense, it is accidental that it is represented by a pointer.

- [What are lifted and unlifted product types in Haskell?](https://stackoverflow.com/questions/39985296/what-are-lifted-and-unlifted-product-types-in-haskell)

## Unboxed tuples

`Int#` や `Double#` などの `unboxed primitive tpyes` に加えて、`Haskell` は `unboxed tuples` をサポートしている。通常の `boxed tuple` は `(Int, Bool)` といった型であり、これはヒープに確保されたタプル要素へのポインタのベクターとして表される。よって、`boxed tuple` の全ての要素は `boxed` となる。そのため、`Boxed tuples` は `lazy` であるが、設計としては自由選択で良い。

関数から返ってくる複数の値をサポートするために考えられた既存の方法を使えば、`unboxed tuple` は複数の値同士を結びつけるための、単なる `Haskell` の構文にすぎない。`Unboxed tuples` はどんな場合でも実行時では存在しない。例えば

```haskell
divMod :: Int -> Int -> (# Int, Int #)
```

`divMod` は2つの整数を返す。`Haskell` プログラマーは `divMod` を以下のように利用することがある

```haskell
case divMod n k of (# quot, rem #) -> ...
```

`case` を使ってタプルの構成要素を分解する。しかしながら、コンパイルの段階において、`unboxed tuple` は完全に消えてしまっている。`divMod` 関数は分離されたレジスタに保存された2つの値を返すコードにコンパイルされる。`case` 文はそれら2つの値を `quot` と `rem` に単純に束縛するだけのコードにコンパイルされる。これは、タプルのヒープ確保と間接参照を回避できるため、同等の `boxed tuples` バージョンよりも効率的である。

`GHC` のモダンバージョンでは、`unboxed tuples` は関数の引数のように用いることを許可している。`(+) :: (# Int, Int #) -> Int` 関数はコンパイルすると `(+) :: Int -> Int -> Int` と全く同じコードを生成する。そのため、`unboxed tuple` は複数のレジスタを経由して複数の引数を受け渡すことを、単に表すために利用される。

`unboxed tuples` の興味深い側面として、本論文の展開として重要な事柄がある。それは、ネスティングは計算上無関係となる事実である。

```
(# Int, (# Float#, Bool #) #)
(# Int, Float#, Bool #)
```

上記の2つの値の型は確かに異なるが、実行時では両者を同一視することができる。なぜなら、どちらも、3つのレジスタを経由して、返ってきた (または渡された) 3つの値を表現しているからである。

# 3. Unboxed types and polymorphism
イントロダクションで定義した `bTwice` を思い出そう。

``` haskell
bTwice :: forall a. Bool -> a -> (a -> a) -> a
bTwice b x f = case b of True  -> f (f x)
                         False -> x
```

多相的な言語を扱う、多くのコンパイラのように、`GHC` は `x::a` がヒープポインタによって統一された表現として、多相型の値を家庭している。そうすると、 `bTwice` は `x::Int#` や `x::Float#` や `x::(# Int, Int #)` に対して呼び出すことができない。実際に、 `bTwice` は `ByteArray#` のような `boxed unlifted value` にすら利用できない。なぜだめなのだろうか？それは、 `a` が `(f (f x))` の呼び出しで `unlifted` であれば、 `call-by-value` を利用してコンパイルすべきであり、一方で、 `a` が `lifted type` であれば、 `call-by-need` でコンパイルすべきである。

`GHC` はそのため、以下の原理を採用する。

- `The Instantiation Principle`。`unlifted type` によって、多相型変数をインスタンス化してはならない。

これは、プログラマにとっては厄介であるが、堅実な効率の保証を得られる。(代替案としては、8章で議論する何らかの `auto-specialization` となるだろう)。しかしながら、この `instantiation principle` を撤廃することが、超絶技巧の結果であると誤解されないように、我々は残りの章を苦労して書き上げた。

## 3.1 Kinds
コンパイラはどのようにして、`instantiation principle` を実装しているのだろうか？例えば、型が `unlifted` だとわかっている場合にどんな処理を行うか？

`Haskell` は項を型によって分類するように、`kind` によって型を分類する。例えば<sup>5</sup>、`Bool :: Type`, `Maybe :: Type -> Type`, `Maybe Bool :: Type` などだ。そのため、`kind` を用いることで自然に型を `lifted` と `unlifted` の形式に分類できるため、`Int# :: #`, `Float# :: #` となる。ただし、 `#` は `unlifted type` を分類するための新しい `kind` である。

`unlifted type` では `#` を用いたが、`lifted type` では `Type` を用いる。`laziness` のため、`lifted type` の値はヒープへのポインタによって統一的に表現されなければならない。そのため、`Instantiation Principle` は次のように読み替えることができる。`全ての多相型変数は `Type kind` を持つ。例として、`kind` を明示的に指定した `bTwice` を示す。

```haskell
bTwice :: forall (a :: Type). Bool -> a -> (a -> a) -> a
```

ここで、`Float# :: #` 型をインスタンス化しようとすると、`kind` エラーが発生する。なぜなら、`Type` と `#` は異なる `kind` だからだ。

------

5. `Haskell Report`[9] では任意の型の `kind` を `*` 記号を使って表している。しかし、コミュニティでは新しいスペル `Type` が使われ始めている。これは `GHC 8` で利用可能である。我々は本論文において、`*` の代わりに `Type` を用いる。

## Sub-kinding
Haskell はリッチな型言語である。特に興味深いのは、アロー関数 `(->)` が以下の `kind` で `binary type constructor` となる点である。

```hasell
(->) :: Type -> Type -> Type
```

`(->)` の部分適用は時に有用である。

```haskell
instance Monad ((->) env) where
  -- note that env is the left argument to (->)
  return x = ¥env -> x
  ma >>= fmb = ¥env -> fmb (ma env) env
```

上述の `kind` によって、`Monad ((->) env)` 型は `well-kinded` である。

しかし、ここで深刻な問題に直面する。`sumTo# :: Int# -> Int# -> Int#` のような `unlifted type` を扱う関数の場合、`ill-kinded` となってしまう。なぜだろうか？それは、`(->)` は `Type` を期待しているが、`Int# :: #` となっているためである。この問題は `unboxed value` の時からずっと `GHC` について回っている。長い期間、この問題に対する解決策は、以下に図示するような `sub-kinding` 関係によってサポートすることだった。

![sub-kinding relation](https://github.com/waddlaw/GHC8.2.1-survey/blob/master/levity/images/fig1.png "sub-kinding relation")

`GHC` には `Type` と `#` の `super-kind` として `OpenKind` がある。

不確かな `sub-kinding` のコンパイル処理を避けるために、 `kind inference` を行う。`GHC` はこの奇妙な `OpenKind` を引数に取る `(->)` について、`fully-saturated` な場合のみに利用することを明記している。仮に `(->)` を部分適用したとすると、上述した `kind` とは程遠い `Type -> Type -> Type` となってしまう。

`Haskeller` はこの手品のような仕組みを受け入れる代わりに、次のような代償を支払っている。

- 型理論の分野に明るく、物事をきっちりする学生が、メーリングリストに突然現れ、なぜこんな動作になるのか？ `(->) :: Type -> Type -> Type` のとき、 `GHC` は `Int# -> Double#` のような型を受理するのか？
- これは、(a) 型推論, (b) ポリモーフィズム, (c) サブタイピングのコンビネーションとしてよく知られている問題である。そして、`GHC` の型推論の実装では、素晴らしい部分と `sub-kinding` による原理化されていない特殊なケースで穴だらけになっている。
- `kind polymorphism`[17] のイントロダクションでこのシチュエーションが最悪だと述べている。また、その後の `kind equalities`[16] のイントロダクションでは、全くもって論理的ではないと述べられている。
- `OpenKind` kind はエラーメッセージの出現をわかりづらくする

結局のところ、`sub-kinding` は全くもって満足いく解決策ではなかった。そのため、何か別の良い解決策を探すようになった。

## 3.3 Functions that diverge

# References

- [9] S. Marlow (editor). Haskell 2010 language report, 2010.
    - [Haskell 2010 Language Report (pdf)](https://www.haskell.org/definition/haskell2010.pdf)
    - [Haskell 2010 Language Report (online)](https://www.haskell.org/onlinereport/haskell2010/)
- [12] S. Peyton Jones and J. Launchbury. Unboxed values as first class citizens. In *FPCA*, volume 523 of *LNCS*, pages 636-666, 1991.
    - [Unboxed values as first class citizens in a non-strict functional language](https://www.microsoft.com/en-us/research/wp-content/uploads/1991/01/unboxed-values.pdf)
- [16] S. Weirich, J. Hsu, and R. A. Eisenberg. System FC with explicit kind equality. In *International Conference on Functional Programming*, ICFP '13. ACM, 2013.
    - [System FC with Explicit Kind Equality](http://www.cis.upenn.edu/~justhsu/docs/nokinds.pdf)
    - [System FC with Explicit Kind Equality (Extended Version)](http://repository.brynmawr.edu/cgi/viewcontent.cgi?article=1014&context=compsci_pubs)
- [17] B. A. Yorgey, S. Weirich, J. Cretin, S. Peyton Jones, D. Vytiniotis, and J. P. Magalhaes. Giving Haskell a promotion. In *Types in Language Design and Implementation*, TLDI '12. ACM, 2012.
    - [Giving Haskell a Promotion](http://www.seas.upenn.edu/~sweirich/papers/tldi12.pdf)
