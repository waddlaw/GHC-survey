- [NoSubKinds](https://ghc.haskell.org/trac/ghc/wiki/NoSubKinds)

このページは `Simon PJ` のアイデアをもとに `GHC` の部分カインド付け (sub-kinding) を除去するための一連の記録である。
`Richard E.` はボランティアで実装を手伝った。

注意: このページの内容は既に実装済みであり、実際の出来事をページの上から下に時系列順で記述した。

# The current story

現在、様々な **基本** カインド ("base" kinds) が存在する。

- `*`: lifted boxed types のカインド。これは、`*` カインドの型がボトム (bottom) を含み、ポインタによって表現される。
- `#`: unlifted types のカインド。このカインドの型はボトムにはなれない。`Int#` や `Double#` など、多くの unlifted type は unboxed だが `Array#` のように boxed (ヒープポインタで表現される) な場合もある。
- `Constraint`: Haskell の型クラス制約のカインド。凄く厄介。
  - 型チェックの間は `*` と `Constraint` は完全に異なるカインドでなければならない。そのため、型シグネチャ `f :: Int => Int` はリジェクトされる。
  - Core では `Constraint` と `*` を同等に扱うという点がとても重要である。**SLPJ** なぜ?以前は `GND` が辞書の役割となっていたため、そのように利用していたが、それ以上のことは何もしていない。**RAE** 我々は1メンバクラスを強制的に変換する時にこのトリックをまだ使っている。正しいよね？さらに、`*` と `Constraint` が異なっていれば、複数の異なるアローを必要とするだろう。どんな場合でも、この問題と今回の提案は完全に関係の無いものだ。

  そのため、`tcEqType` は `Constraint` と `*` を区別 (Haskell の中での区別) するが、 `eqType` は両者を等しいものとして扱う。

- `OpenKind`: `*` と `#` のスーパーカインド。`OpenKind` は様々な理由で存在している。
  - `(->)` にカインドを与え、`OpenKind -> OpenKind -> *` として使うため。**SLPJ** 違う。我々は `(->)` に対して実際に `* -> * -> *` を与えている。ただ、 `(->)` は部分適用しないという特別なカインド規則がある。**RAE** はい。しかし、以下の立場に立ってみたら違うのでは？
  - `error :: forall (a :: OpenKind). String -> a` と `undefined :: forall (a :: OpenKind). a)` に型を与えるためである。`error Int# "foo" :: Int#` という形を許容したい。
  - 推論において、ラムダに束縛された変数にカインドを与える。例: `\x -> 3# +# x` 。ラムダ式が出現した時、アルファの型を与え、ユニフィケーション変数とする。しかし、アルファ型のカインドは何にすれば良いのだろうか？`*` では無いため、ラムダ式はリジェクトされてしまう。そのため、ここで `OpenKind` を与える。
  - `BOX`: カインドの分類。すなわち、 `* :: BOX`, `# :: BOX` であり、ややこしいことに `BOX :: BOX` である。

`OpenKind` の全体的な動作は満足いくものではない。例として、このような抽象化はできない。

```haskell
myError s = error ("Blah" ++ s)
```

今の所、`myError` は `Int#` 型では利用できない。

# Down with kinds
以下に提案された `remedy` は、カインドを分類するための階層 (`BOX`) と関連するダメな部分を直すためのものである。現在の GHC に実装するのではなく、Richard は自身のブランチに `BOX` を完全に除去したものを実装した。それどころか、 `* :: *` やコアにおける型とカインドを区別するものを無くした。詳細については[論文](http://cs.brynmawr.edu/~rae/papers/2013/fckinds/fckinds.pdf)を参照せよ。

これ以降の内容は型とカインドがマージされた言語とセマンティクスを想定していることに注意せよ。

# Proposed remedy
私たちは `Levity` という普通のデータ型を定義した。

```haskell
data Levity = Lifted | Unlifted
```

そして、`Levity -> TYPE Lifted` の型となるような新しいマジカル定数 `TYPE` を生成した。このアイデアは `TYPE Lifted` を古い `*` へ、 `TYPE Unlifted` を古い `#` に置き換えることである。その結果、仮に `* :: *` だった場合、 `TYPE Lifted` を意味するカインドに上手く分類できる。こうして、奇妙な型 `TYPE` が導入された。これで `OpenKind` は `forall (l :: Levity). TYPE l` のように置き換えることができる。実際の型をいくつか示す。

```haskell
(->) :: forall (l1 :: Levity) (l2 :: Levity). TYPE l1 -> TYPE l2 -> TYPE Lifted
error :: forall (l :: Levity) (a :: TYPE l). String -> a
undefined :: forall (l :: Levity) (a :: TYPE l). a
```

サブカインド物語は完全に上位互換なカインドポリモーフィズムの登場によって終焉を迎えた。ユーザのために以下を定義した。

```haskell
type * = TYPE Lifted
type # = TYPE Unlifted
```

さらに、GHCの既存の `preserve-type-synonyms-wherever-possible` 機構がメッセージを保存することは簡単である。

この提案は `Constraint` / `*` 問題には言及していないことに注意せよ。 -- これは別の問題である。

## Levity polymorphism
ここで型システムに *levity polymorphism* をサポートしたいという方針となった。これは型システムが `\x -> x :: forall (v :: Levity) (a :: TYPE v). a -> a` のような形式をサポートすることを意味する。しかし、これは非常に馬鹿げた話だった。 -- コード生成器はポインタを扱うのかそうでないかを知る必要がある!そのため、一般的な levity polymorphism は禁止したかった。

しかし、時には限定された levity polymorphism を使いたいときがある。例を示す。

```haskell
myError :: forall (v :: Levity) (a :: TYPE v). String -> a
myError s = error ("Me" ++ s)
```

上記の宣言は受理されるべきであるが、これは完全に levity-polymorhic である。

Simon と Richard は以下のルールによって levity polymorphism が動作するだろうと考えていた。levity polymorphism は以下の条件を満たす場合のみ受け入れることとする。

- ユーザは型シグネチャによって levity polymorphism を特別にリクエストできる
- Levity-polymorphic 型変数は矢印の右側にのみ出現することができる

1つ目のポイントは, GHC は余計なことはしない。例えば以下のコードはとても良い。

```haskell
f :: forall (v :: Levity) (a :: *) (b :: TYPE v). (a -> b) -> a -> b
f g x = g x
```

しかし、ユーザが型推論を望むのであれば、`an exotic specimen` のようにプロデュースしてはいけない。

2つ目のポイントは levity polymorphism を許可するための正しい方法だと思う。もし、levity-polymorphic 変数が矢印の右側にのみ言及するのであれば、parametricity によって、値の無い変数がどれかわかる。すなわち、関数は分岐しなければならい (または `error` か `undefined` を呼び出す。levity-polymorphic 型変数のための2つのプリミティブな値)。これは実装される前に、より深く考察する価値がある。

実際の実装は簡単であるべきだ。`checkValidType` において、TYPE のパージングをサポートし、levity-polymorphism をチェックする。推論アルゴリズムは変更するべきではなく、levity-polymorphic 型として絶対に推論されず、チェックだけ行われる。この拡張は今後の作業として残っている。

## Issues from Dimitrios

**Apr 15**: Richard の実装に対して、以下の issue が全て提案された。

- 私たちは、どんな型が OK か Lint チェックする必要がある。つまり、FC プログラムは正しく型付けされる。
- 判断の基準の提案: `(ty:TYPE l)` は **矢印の左側には出現できない。** また `(ty:TYPE l)` は **任意の束縛の型の中には出現しない** 。そのため、コンストラクタのフィールドには出現できない。なぜなら、コンストラクタの型が正しく型付けされないため。
- `(Int -> (ty:TYPE l)) -> Int` は OK
- `((ty:TYPE l) -> Int) -> Int` は NG
- `data T l (a:TYPE l) = MkT (Int -> a)` は OK
- なので `MkT Unlifted Int# (\n -> error Unlifted Int# "urk")` は OK
- さらに `undefined` のトップレベル定義も OK。なぜなら、`undefined :: forall v. forall (a:TYPE v). a` となるため。

You make it sound as if the only problem is defaulting. But I do not think it is enough. For instance, what prevents a user from writing:
唯一の問題は defaulting の問題だけだと思うかもしれないが、私はそれだけではないと思う。
例えば、ユーザが以下のように記述してしまうことを防ぐためにはどうしたら良いだろう。

```haskell
f :: forall v. forall (a :: TYPE v). a -> a 
f x = x
```

この問題に対するもっともらしい唯一の説明は、TYPE と levity はプログラマに公開されないのでそんなコードは書けないということである。
\*しかし\* TYPE と levity をプログラマに公開しなければならないときにこの問題は顕在化する。
例えば "error" のイータ拡張のためにシグネチャを与えなければならない場合である。
例: 

```haskell
g :: forall (a:*). forall v. forall (b :: TYPE v). Show a => a -> b
g x = error (show x) 
```

このように、"GHCで問題になるのは defaulting だけだ" という主張は本来抱えている問題の一部分にすぎない。
どのようにして、悪い "f" を防ぎ、良い "g" を許可するのだろうか？

現在、私は defaulting が解決策の一部であることは認めるが、それが矢印の左側で levity の抽象化が行われないことを本当に保証できるのだろうか？
例えば、仮に以下のように (->) が型付けされたとしよう:

```haskell
(->) :: forall v1,v2, FIXED v1 => TYPE v1 -> TYPE v2 -> *
```

ここで `FIXED v1` はカインド推論によって決まるカインドクラス制約から導出されるようなもの (しかし、もちろんそれと関連付けられた証拠は存在しない) であり、一般化された levity 変数を防ぐ。
そのため、型推論は次のようになる:

```haskell
FIXED Lifted   => is just discharged successfully
FIXED Unlifted => is just discharged successfully
```

しかし、型推論の最後に次の形式の制約が残されている:

```haskell
FIXED levity_unification_variable  ==> can be defaulted to Lifted
                                               (this will allow us to infer sound types)
FIXED levity_skolem_variable       ==> are genuine errors! 
```

私は `FIXED` カインドクラスのようなカインド制約を生成することが難しいとは思わないし、defaulting の特別なものとして扱えるのだが、どうだろうか？
実際のところ、もしかすると、カインド等価性の完全な動作に頼らない形で \*今日の\* GHC に導入したいのではないかと推測している。

面白い例を示す。

```haskell
g :: (forall a. (# a, a #)) -> (# Int, Int #)
{-# NOINLINE g #-}
g x = x

foo _ = g (# error "a", error "b" #)
```

結論:
- `forall (l:Levity). ty` はカインド `*` である。我々はそれを値抽象パイとして考える。`undefined :: forall l. forall (a:Type l). a` の全体の型のカインドは `*` であり、 lifted である。
- forall の本体は、ある levity `l` となる `Type l` カインドとなるべきであり、それが forall のカインドとなる。これは、本体がカインド `* -> *` とならないことを意味し、間違いなく `k` ではない。これは、正しく無いカインド `forall k (a::k). a` となる。 (Cf [#10114](https://ghc.haskell.org/trac/ghc/ticket/10114))


# Implementation

**Feb 24, 2016**

この一般化されたアイデアはバージョン 8.0 に実装されたが、少し工夫した点があり、そこから全てが始まった。

```haskell
type family F a where
  F Int# = Int
```

とてもわかりやすい。
型族を使えば unlifted type を上手く処理できた。

しかし、これは？

```haskell
type family G a :: k where
  G Int = Int#
  G Bool = Int

foo :: G a -> ...
```

`G a` のカインドは `TYPE r` となるはずだが、それは `foo` の引数のサイズがわからないことを意味する。つまり大失敗だ。
仮に `k` が `#` とわかっていたとしても、まだこの問題は解決しない。なぜなら、unboxed 型によって多数の異なるサイズがあるからだ。

そのため、こうした。

```haskell
data RuntimeRep = PtrRepLifted | PtrRepUnlifted | IntRep | VoidRep | ...
TYPE :: RuntimeRep -> *
type * = TYPE 'PtrRepLifted
```

これは、上述のアイデアと似ているが levity の代わりに `RuntimeRep` を利用している。
現在は、型のカインドから常に値のサイズがわかるようになった。
実際の定義は `GHC.Types` にある。

我々は今のところ、全ての束縛された変数が固定したサイズを持つことを保証しなければならない。
これは大抵何もしなくて良い。なぜなら constraint-solving が `RuntimeRep` に通知できるからである。
束縛が終わったら、サイズがわかっているかどうかをチェックし、問題があればエラーにする。


## Alternative RuntimeRep choice
上記の `RuntimeRep` は以下のようにすることもできる:

```haskell
data RuntimeRep = PtrRep Levity | VoidRep | IntRep | ...
data Levity = Lifted | Unlifted
```

現在、levity を与えることなく、確かな型を boxed だと言うことができる。
これはとても良い。なぜなら boxed unlifted のポリモーフィズムが賢明だと思う。
これは、boxed と unboxed の両方になるような (恐怖となる) ものを防ぐために、より制限した型を `unsafeCoerce#` に与えることができる。
(仮にこれを行うと、本当に制限されていない `reallyUnsafeCoerce#` や `unsafeCoerce##` のようなものを導入するでしょう。)
しかしながら、これは多レイヤアプローチが体感できるほどのパフォーマンスに影響を与えます (プログラムごとに影響度は違う)。なぜなら、`*` を剥がす時にポインタを捕まえる必要があるため。
unpacked sum を使う時、これを再検討する。なぜなら、理論的な観点から見ればこの代替案はより良いからだ。
