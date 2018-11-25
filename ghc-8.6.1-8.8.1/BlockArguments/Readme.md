# BlockArguments

- [ghc-proposals/ghc-proposals #90](https://github.com/ghc-proposals/ghc-proposals/pull/90)
- [0010-block-arguments.rst](https://github.com/ghc-proposals/ghc-proposals/blob/36070b13d3f0970cda1faebc76afc220483340d6/proposals/0010-block-arguments.rst)
- [BlockArguments - GHC manual](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-BlockArguments)

## 言語拡張

```hs
{-# LANGUAGE BlockArguments #-}
```

で有効化できる。

関数の引数に、以下のブロックを適用できる。

- do expressions
- lambda expressions
- `if`
- `case`
- `let`
- `mdo` (recursive do-notation)
- `\case ` (Lambda-case)
- `proc` (Arrow notation)

## 例

```hs
atomically $ do
  v <- readTVar tv
  writeTVar tv $! v + 1
```

こう書けるようになる。

```hs
{-# LANGUAGE BlockArguments #-}
atomically do
  v <- readTVar tv
  writeTVar tv $! v + 1
```

## パーズのされ方

### 例1

```hs
when (x > 0) do
  print x
  exitFailure
```

```hs
when (x > 0) (do
  print x
  exitFailure)
```

### 例2

```hs
withForeignPtr fptr \ptr -> c_memcpy buf ptr size
```

```hs
withForeignPtr fptr (\ptr -> c_memcpy buf ptr size)
```

## 文法の変更

```hs
lexp  →  \ apat1 … apatn -> exp            (lambda abstraction, n ≥ 1)  *
      |  let decls in exp                  (let expression)             *
      |  if exp [;] then exp [;] else exp  (conditional)                *
      |  case exp of { alts }              (case expression)            *
      |  do { stmts }                      (do expression)              *
      |  fexp

fexp  →  [fexp] aexp                       (function application)

aexp  →  qvar                              (variable)
      |  gcon                              (general constructor)
      |  literal
      |  ( exp )                           (parenthesized expression)
      |  qcon { fbind1 … fbindn }          (labeled construction)
      |  aexp { fbind1 … fbindn }          (labelled update)
      |  …
```

```hs
lexp  →  fexp

fexp  →  [fexp] aexp                       (function application)

aexp  →  qvar                              (variable)
      |  gcon                              (general constructor)
      |  literal
      |  ( exp )                           (parenthesized expression)
      |  qcon { fbind1 … fbindn }          (labeled construction)
      |  aexp { fbind1 … fbindn }          (labelled update)
      |  \ apat1 … apatn -> exp            (lambda abstraction, n ≥ 1)  *
      |  let decls in exp                  (let expression)             *
      |  if exp [;] then exp [;] else exp  (conditional)                *
      |  case exp of { alts }              (case expression)            *
      |  do { stmts }                      (do expression)              *
      |  …
```


## 注意点

`f \a -> a b` will be parsed as `f (\a -> a b)`, not as `f (\a -> a) b`.
