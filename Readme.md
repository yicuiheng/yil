## プログラミング言語 Yil

Yil は 易翠衡 (yicuiheng) が作っているプログラミング言語です．  
Yil は篩型という表現力の高い型システムを持っています．  

例えば次の偶奇を判定するプログラム `example-codes/well-typed/even_odd.yil` で Yil が何を型で表現できるのか見てみましょう．
```
rec func is_even (n:int | n >= 0) :
  (ret: bool | ((n % 2 = 0) => ret) && ((n % 2 != 0) => !ret)) =

  if n = 0 { true }
  else { is_odd (n - 1) }

rec func is_odd (n:int | n >= 0) :
  (ret: bool | ((n % 2 = 0) => !ret) && ((n % 2 != 0) => ret)) =
  
  if n = 0 { false }
  else { is_even (n - 1) }

func main (a:int | true): (ret:int | true) =
   let unused = print_bool (is_even 42) in
   0
```

関数 `is_even` は非負整数 `n` を受け取って `n` が偶数なら `true` を，奇数なら `false` を返す関数です．  
Yil ではこの *`n` が非負* であるという条件や *`n` が偶数なら ...* といった条件を型の中で表現しています．
具体的にはプログラム中の `(n: int | n >= 0)` は引数 `n` が 0 以上の整数であることを表し，`(ret: int | ((n % 2 = 0) => ret = true) && ((n %2 != 0) => ret = false))` は 引数 `n` が偶数なら `true` を返し奇数なら `false` を返すことを表しています．  

このように Yil では型に論理式をつけることでプログラムの仕様をより詳細に記述でき，プログラムが仕様を満たしているかどうかを型検査器が（多くの場合）自動で検証してくれるのです！！  


では，もし関数の実装が型の中に記述されている制約を満たさない場合，どうなるのでしょうか．間違った実装の偶奇判定プログラムを `example-codes/ill-typed/even_odd.yil` に用意しました．
```
rec func is_even (n:int | n >= 0) :
  (ret: bool | (n % 2 = 0) => ret && (n % 2 != 0) => !ret) =

  if n = 0 { true }
  else { is_odd (n - 1) }

rec func is_odd (n:int | n >= 0) :
  (ret: bool | (n % 2 != 0) => ret && (n % 2 = 0) => !ret) =
  
  if n = 0 { true }
  else { is_even (n - 1) }

func main (a:int | true): (ret:int | true) =
   let unused = print_bool (is_even 42) in
   0
```

はじめに出した正しい実装の偶奇判定プログラムとの違いは `is_odd` 関数の `if` 式のいわゆる then 節 の式が `false` ではなく `true` であるという点です．
おそらく `is_odd` 関数を実装する際に `is_even` 関数をコピペして then 節を修正するのを忘れたのでしょう．  
これを実行すると以下の出力を得ます．
```
[type error] given implementation does not meet specification
   |
10 |   if n = 0 { true }
   |   -----------------
11 |   else { is_even (n - 1) }
   | -------------------------- implementation
  vs.
  |
8 |   (ret: bool | (n % 2 != 0) => ret && (n % 2 = 0) => !ret) =
  |   -------------------------------------------------------- specification

counter example [
  n = 0,
]
```

これは `if` 式の実装が関数の返り値に対する仕様を満たさないということを言っています．
さらにこの仕様違反が起きるのはどういった時かも教えてくれていて，ここでは引数 `n` が `0` の時だそうです．確かに `n` が `0` の時この関数 `is_odd` は `true` を返しますが 0 は奇数ではないですね．
