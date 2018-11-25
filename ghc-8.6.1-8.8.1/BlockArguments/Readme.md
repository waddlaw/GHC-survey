# BlockArguments

- [ghc-proposals/ghc-proposals #90](https://github.com/ghc-proposals/ghc-proposals/pull/90)
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
