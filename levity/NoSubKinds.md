- [NoSubKinds](https://ghc.haskell.org/trac/ghc/wiki/NoSubKinds)

このページは、`Simon PJ` のアイデアをもとに、 `GHC` の部分カインド付け (sub-kinding) を取り除くための設計に関する計画の記録である。 `Richard E.` はボランティアで実装を手伝った。

注意: このページの事柄は既に実装済みである。実際に何が起きたのか、ページの上から下に時系列順となっている。

# The current story

今現在、様々な**基本**カインド ("base" kinds) が存在する。

- `*`: lifted boxed types のカインド。これは、`*` カインドを持つ型がボトム (bottom) を含み、ポインタによって表現される。
- `#`: unlifted types のカインド。このカインドを持つ型はボトムにはなれない。`Int#` や `Double#` などの、多くの unlifted type は unboxed であるが、 `Array#` のように boxed (ヒープポインタで表現される) な場合もある。
- `Constraint`: Haskell の型クラス制約のカインド。厄介なことに、
  - `*` と `Constraint` の型チェックは完全に異なるカインドでなければならない。そのため、型シグネチャ `f :: Int => Int` はリジェクトされる。
  - GHC内部において重要な点として、 `Constraint` と `*` は区別の付かないものとして処理する。**SLPJ** なぜ?以前は `GND` が辞書の役割となっていたため、そのように利用していたが、それ以上のことは何もしていない。**RAE** 我々は1メンバクラスを強制的に変換する時にこのトリックをまだ使っている。正しいよね？さらに、`*` と `Constraint` が異なっていれば、複数の異なるアローを必要とするだろう。どんな場合でも、この問題と今回の提案は完全に関係の無いものだ。

  そのため、`tcEqType` は `Constraint` と `*` を区別 (Haskell の中での区別) するが、 `eqType` は両者を等しいものとして扱う。

- `OpenKind`: `*` と `#` のスーパーカインド。`OpenKind` は様々な理由で存在している。
  - `(->)` にカインドを与え、`OpenKind -> OpenKind -> *` として使うため。**SLPJ** 違う。我々は `(->)` に対して実際に `* -> * -> *` を与えている。ただ、 `(->)` は部分適用しないという特別なカインド規則がある。**RAE** はい。しかし、以下の立場に立ってみたら違うのでは？
  - `error :: forall (a :: OpenKind). String -> a` と `undefined :: forall (a :: OpenKind). a)` に型を与えるためである。`error Int# "foo" :: Int"` という形を許容したい。
  - 推論において、ラムダに束縛された変数にカインドを与える。例: `\x -> 3# +# x` 。ラムダ式が出現した時、アルファの型を与え、ユニフィケーション変数とする。しかし、アルファ型のカインドは何にすれば良いのだろうか？`*` では無いため、ラムダ式はリジェクトされてしまう。そのため、ここで `OpenKind` を与える。
  - `BOX`: カインドの分類。すなわち、 `* :: BOX`, `# :: BOX` であり、ややこしいことに `BOX :: BOX` である。

`OpenKind` の全体的な動作は満足いくものではない。例として、このような抽象化はできない。

```haskell
myError s = error ("Blah" ++ s)
```

今の所、`myError` は `Int#` 型では利用できない。

# Down with kinds
以下に提案された `remedy` は、ヒエラルキーの上の段 (`BOX`) と関連するダメな部分を直すためのものである。現在の GHC に実装するのではなく、Richard は自身のブランチに `BOX` を完全に除去したものを実装した。それどころか、 `* :: *` やコアにおける型とカインドを区別するものを無くした。詳細については[論文](http://cs.brynmawr.edu/~rae/papers/2013/fckinds/fckinds.pdf)を参照せよ。

このページの内容は型とカインドがマージされた言語とセマンティクスを想定していることに注意せよ。

# Proposed remedy
私たちは `Levity` という普通のデータ型を定義した。

```haskell
data Levity = Lifted | Unlifted
```

そして、`Levity -> TYPE Lifted` の型となるような、新しいマジカル定数 `TYPE` を生成した。このアイデアは `TYPE Lifted` を古い `*` へ、 `TYPE Unlifted` を古い `#` に置き換えることである。その結果、仮に `* :: *` だった場合、 `TYPE Lifted` を意味するカインドに上手く分類できる。こうして、奇妙な型 `TYPE` が導入された。これで、 `OpenKind` は `forall (l :: Levity). TYPE l` のようにすることができる。特別に実際の型をいくつか示す。

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

# Levity polymorphism
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




The actual implementation should be easy: add parsing support for TYPE and then check for levity-polymorphism in checkValidType. The inference algorithm should remain unchanged, as a levity-polymorphic type would never be inferred, just checked. This extension is left as future work.
