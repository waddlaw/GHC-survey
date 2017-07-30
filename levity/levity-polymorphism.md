# Levity Polymorphism (extended version)

- [paper](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/levity-1.pdf)
- [NoSubKinds](https://ghc.haskell.org/trac/ghc/wiki/NoSubKinds)
    - [日本語訳](/levity/NoSubKinds.md)
- [What is Levity polymorphism](https://stackoverflow.com/questions/35318562/what-is-levity-polymorphism/35320729#35320729)
- [The data type Type and its friends](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/TypeType)
- [Levity polymorphism](https://ghc.haskell.org/trac/ghc/wiki/LevityPolymorphism)

## 対訳表

アルファベット順

| Term | 用語 |
| :---- | :----- |
| abstract representation | 抽象表現 |
| boxed type | boxed type|
| colling convention | 呼出規約 |
| kind | カインド |
| levity polymorphism | levity porimorphism |
| polymorphic | 多相的 |
| polymorphism | ポリモーフィズム |
| sub-typing | サブタイピング |
| unboxed type | unboxed type |
| unboxed value | unboxed value |



# Abstract

# 1. The cost of polymorphism

以下のような `Haskell` の関数を考察する。

``` haskell
bTwice :: forall a. Bool -> a -> (a -> a) -> a
bTwice b x f = case b of True  -> f (f x)
                         False -> x
```

この関数は `a` について多相的 (`polymorphic`) [<sup>1</sup>](#note-1) である。多相的とは、`x` の型に関わらず、 `f` 及び `a` を入れ替えても、同一の関数で動作することを言う。我々が "同じ関数" と言う時は、常に "`bTwice` をコンパイルした同一のコードが、任意の型の引数 `x` に対して動作することを言う"。しかし、`x`の型の呼出規約 (colling convention) に影響される。それゆえ、`bTwice` の実行可能コードとなる。例えば、仮に `x` がリストだったとしよう。これは、ヒープを指し示すレジスタに渡される。倍精度浮動小数点数であれば、特別な浮動小数点レジスタに渡されるだろう。すなわち、ポリモーフィズムでは共有コードが衝突する。

単純で広く利用されている解決策としては、全ての値をヒープに確保されたオブジェクトへのポインタとして表現することである。この手法の欠点は非常に処理が遅い点にある ([2.1章](#21-unboxed-values))。そのため、多くの多相的な言語ではポインタとしての表現ではなく、値自身を表す `unboxed values` の形式をサポートしている。Glasgow Haskell Compiler (GHC) に関して言えば、`Haskell` の最先端の最適化である。GHC は何十年前から unboxed value をサポートしているが、unboxed value とポリモーフィズムの間に避けられない緊張関係が生じている ([3章](#3-unboxed-types-and-polymorphism))。他の言語では異なる手法でこの問題に取り組んでいる ([8章](#8-polymorphism-in-other-languages))。

本論文では、既に普及しているポリモーフィズムにおいてハイパフォーマンスを実現させるための斬新で新しい手法を提案する。以下に概要を示す。

- 型をカインド (kind) で分類することによる、多相的な関数とデータ型のコンパイルに関する原理的な推論方法を提案する ([4章](#4-key-idea-polymorphism-not-sub-kinding))。それぞれのカインドは型のメモリレイアウトについて述べることで、それらの型を多相的に扱う関数の呼出規約を決定する。
- メモリレイアウトと呼出規約の記述方法について原理化した上で、一歩先に進み `levity polymorphism` を採用する。これにより、固定された値か格納されたデータかを表す抽象表現によって、メモリレイアウトの選択が抽象化された (`abstracted`) 関数を許可できる ([5章](#5-taming-levity-polymorphism))。我々が始めて levity polymorphism を記述・実装したと信じている。
- levity polymorphism は巧妙な手法ではなく、本当に厳密であり、受け入れても良いと思ってもらえるだろう。levity-polymorphic function が実際に具体的なコードへコンパイルされることを保証するために、我々の規則が十分であることを形式的に証明する。([6章](#6-correctness-of-levity-polymorphism))
- 獲得した levity polymorphism には、`(->)` のカインドの非形式的な記述や、原理的な方法によって、boxed type と unboxed type の操作のオーバーロードなど、様々な可能性を秘めている。([7章])(#7-new-opportunities-from-levity-polymorphism) で詳細に議論する。

Levity polymorphism は2016年の初めにリリースされた GHC バージョン 8.0.1 で実装された。我々はこの手法でカインドを利用することは実は初めてではない。Cyclone[[5]](#5) は8.1章と同様の手法を利用している。しかし、我々が知る他のどんなコンパイラよりも洗礼されたアイデアを採用し、最高の結果となった。忘れないで欲しいことは、これは全てパフォーマンスに関する内容である。もし、パフォーマンスを気にする必要が無ければ人生はもっと楽なのだが!
 
------

<a name="note-1">1</a>. 本論文で `polomorphism` という用語を用いる場合は全て `parametric polymorphism` の意味である。

# 2. Background: performance through unboxed types
本論文が取り組むパフォーマンスへの挑戦を最初に記述する。我々はこれからの議論を具体的にするため `Haskell`[<sup>2</sup>](#note-2) と `GHC` コンパイラを用いるが、多くの事柄が、他の多相的な言語にも同様に適用できる。他の言語とコンパイラについては[8章](#8-polymorphism-in-other-languages)で議論する。

-----
<a name="note-2">2</a>. `GHC` はハイパフォーマンスコードをサポートするために、様々な方法で `Haskell` を拡張している。そのため、`Haskell` と記述されている部分は `GHC Haskell` の略だと思って欲しい。

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

`Int` を使って `sumTo` を計算すると、驚くほど遅い。それぞれの繰り返しが第二引数[<sup>3</sup>](#note-3)を評価しようとするためだ。

- 基底部では `sumTo` の第二引数を評価し、`ポインタをたどって` 値を取得し、0かどうか確認する必要がある。
- 再帰部では、`(acc + n)` と `(n - 1)` のための `サンクを確保` し、再びそれを繰り返す。

これに対して、`C` コンパイラは `a three-machine-instruction loop` を利用するため、メモリへのアクセスは全く発生しない。
これが、パフォーマンスに莫大な差を与える。

そのため、`GHC` では `unboxed integer` を表す `Int#` 型が組み込みで提供されている[[12]](#12)。
`Int#` はポインタによる表現ではなく、整数値自身である。

`unboxed integer` を使った `sumTo` の実装は次のようになる。[<sup>4</sup>](#note-4)

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

<a name="note-3">3</a>. 思い出して欲しい、`Haskell` は遅延言語である。そのため、第二引数は必要となるまで評価されない。

<a name="note-4">4.</a> 接尾辞 `#` はコンパイラによって一切特別な処理が加えられることはない。これは、 `unboxed` な値であることを読者に示唆するための素朴な命名規則である。

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

`Haskell` は項を型によって分類するように、`kind` によって型を分類する。例えば[<sup>5</sup>](#note-5)、`Bool :: Type`, `Maybe :: Type -> Type`, `Maybe Bool :: Type` などだ。そのため、`kind` を用いることで自然に型を `lifted` と `unlifted` の形式に分類できるため、`Int# :: #`, `Float# :: #` となる。ただし、 `#` は `unlifted type` を分類するための新しい `kind` である[<sup>6</sup>](#note-6)。

`unlifted type` では `#` を用いたが、`lifted type` では `Type` を用いる。`laziness` のため、`lifted type` の値はヒープへのポインタによって統一的に表現されなければならない。そのため、`Instantiation Principle` は次のように読み替えることができる。全ての多相型変数は `Type kind` を持つ。例として、`kind` を明示的に指定した `bTwice` を示す。

```haskell
bTwice :: forall (a :: Type). Bool -> a -> (a -> a) -> a
```

ここで、`Float# :: #` 型をインスタンス化しようとすると、`kind` エラーが発生する。なぜなら、`Type` と `#` は異なる `kind` だからだ。

------

<a name="note-5">5</a>. `Haskell Report`[[9]](#9) では通常の型の `kind` を `*` 記号を使って表している。しかし、コミュニティでは新しいスペル `Type` が使われ始めている。これは `GHC 8` で利用可能である。我々は本論文において、`*` の代わりに `Type` を用いる。

<a name="note-6">6</a>. この一貫性の無い `#` 記法で取り乱さないでいただきたい。実際に過去の `GHC` で利用されていたものだが、本論文の残りの部分で、より良い統一方法を示す。



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
- `kind polymorphism`[[17]](#17) のイントロダクションでこのシチュエーションが最悪だと述べている。また、その後の `kind equalities`[[16]](#16) のイントロダクションでは、全くもって論理的ではないと述べられている。
- `OpenKind` kind はエラーメッセージの出現をわかりづらくする

結局のところ、`sub-kinding` は全くもって満足いく解決策ではなかった。そのため、何か別の良い解決策を探すようになった。

## 3.3 Functions that diverge

次のような関数を考える。

```haskell
f :: Int# -> Int#
f n = if n <# 0# then error "Negative argument" else n /# 2#
```

ここで `error :: forall a. String -> a` は文字列を表示し、実行を停止する[<sup>7</sup>](#note-7)。
しかし、`Instantiation Principle` によって、この `error` の呼び出しはリジェクトされる。なぜなら、`a` が `Int#` でインスタンス化されてしまうからである。しかし、この場合は、`Instantiation Principle` を壊していないのでこれで良い。なぜ？それは、`error` 関数は `a` 型の値に対して何もせず、単に実行を停止するのみだからである。この動作は `error` の正しい利用法として厄介であり、この方法ではリジェクトされてしまう。そのため、`GHC`が与える `error` の型は `magical type` となっている。

```haskell
forall (a :: OpenKind). String -> a
```

現在、上述したように `sub-kinding mechanism` を利用し、呼び出しを受理している。この魔法は壊れやすい。もし、ユーザが次のような `error` の変種を記述したらどうだろうか。

```haskell
myError :: String -> a
myError s = error ("Program error  " ++ s)
```

`GHC` は型 `forall (a :: Type). String -> a` を推論するため、魔法はとけてしまう。

------------

<a name="note-7">7</a>. より正確には、例外を投げる。

# 4. Key idea: polymorphism, not sub-kinding

# 5. Taming levity polymorphism

# 6. Correctness of levity polymorphism

# 7. New opportunities from levity polymorphism

# 8. Polymorphism in other languages

# References

[<a name="5">5</a>] D. Grossman. Quantified types in an imperative language. *ACM Trans. Program. Lang. Syst.,* 28(3):429-475, May 2006.
- [Quantified Types in an Imperative Language](https://homes.cs.washington.edu/~djg/papers/qtil.pdf)

[<a name="9">9</a>] S. Marlow (editor). Haskell 2010 language report, 2010.
- [Haskell 2010 Language Report (pdf)](https://www.haskell.org/definition/haskell2010.pdf)
- [Haskell 2010 Language Report (online)](https://www.haskell.org/onlinereport/haskell2010/)

[<a name="12">12</a>] S. Peyton Jones and J. Launchbury. Unboxed values as first class citizens. In *FPCA*, volume 523 of *LNCS*, pages 636-666, 1991.
- [Unboxed values as first class citizens in a non-strict functional language](https://www.microsoft.com/en-us/research/wp-content/uploads/1991/01/unboxed-values.pdf)

[<a name="16">16</a>] S. Weirich, J. Hsu, and R. A. Eisenberg. System FC with explicit kind equality. In *International Conference on Functional Programming*, ICFP '13. ACM, 2013.
- [System FC with Explicit Kind Equality](http://www.cis.upenn.edu/~justhsu/docs/nokinds.pdf)
- [System FC with Explicit Kind Equality (Extended Version)](http://repository.brynmawr.edu/cgi/viewcontent.cgi?article=1014&context=compsci_pubs)

[<a name="17">17</a>] B. A. Yorgey, S. Weirich, J. Cretin, S. Peyton Jones, D. Vytiniotis, and J. P. Magalhaes. Giving Haskell a promotion. In *Types in Language Design and Implementation*, TLDI '12. ACM, 2012.
- [Giving Haskell a Promotion](http://www.seas.upenn.edu/~sweirich/papers/tldi12.pdf)
