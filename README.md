SAnuki Intermediate Language for Udon
=====================================

SAnuki って何？ それはより高級なプログラム言語を作るための "中間言語" です

スタック (Stack) とアドレス (Addresses) の管理を自動化したので SAnuki
それ以外は Udon Assembly (UAssembly) そのままで，for も while も型推論もないのでこいつでプログラム書くのは厳しい

ではなぜ公開するのか？人間でもかろうじて使用可能な最低レベルの言語でも，
できるだけ早めに公開することに意味があると思うからです

質問は [twitter: @cannorin_vrc](https://twitter.com/cannorin_vrc) までお気軽にどうぞ これをベースにまともな言語を作っています


[オンラインコンパイラ](https://7colou.red/sanuki/)があります 右側にリアルタイムで UAssembly に変換されます パースエラー・コンパイルエラーはハイライトされます

コンパイルに成功したらアドレス欄にソースコードがエンコードされて保存されるので，
コピペすれば保存・シェアできます

## チュートリアル (雑)

```
# # で始まるのはコメント

# インデントは自由

# 変数定義ステートメント
#
#   let 名前 : 型 = リテラル
#
let foo : SystemInt32 = 0

# リテラルは
# * 整数 (10進数/16進数)
# * 小数
# * "文字列"
# * this
# * null
# * @<ラベル名>
# の6種類
# 注意:
# * 整数リテラルが使えるのは SystemInt32, SystemUInt32 だけで SystemInt64 とかは使えない
#   - これは変換先である UAssembly のアセンブラがそれしか認識してくれないため クソだね
# * ラベル名は型を SystemUInt32 にする必要がある
# * 文字列は \n 以外のエスケープシーケンスが使えない
#   - これは変換先である UAssembly のパーサが文字列のパースをサボっているため クソだね2

# let に sync<メソッド> を付けると出力に .sync 変数名, メソッド が追記される
#
# sync[Property]<Method> とすると
#   .sync bar->Property, Method
# になる
let sync<SomeMethod> bar : SystemInt32 = 0

# let に pub を付けると出力に .export 変数名 が追記される
# pub と sync 両方使うときは let pub sync<Method> .. と書く
let pub baz : SystemInt32 = 42

# 16進数の整数リテラルは 0x を付ける
let returnAddr : SystemUInt32 = 0x0

# gotoステートメント 指定したラベルにジャンプ
# ラベルは goto より下で定義されていても構わない
goto @_start

# ラベル定義ステートメント
# ラベル名には前に @ を付ける
label @func
  # 変数定義はどこでもできる
  let msg:SystemString = "Hello, World!"

  # call ステートメント extern 関数を呼ぶ
  #
  #   call 関数名 変数1 変数2 ...
  #
  # 引数には変数しか使えない．なぜなら関数によっては最後の引数に与えた変数に結果が代入されたり，途中の引数の値を無造作にいじったりするから
  # 各関数が引数をどう扱うかは関数名からはわからないので覚えるしかない
  # でもどこにも書いてないので SDK をリバースエンジニアリングしない限りわからない クソだね3
  call UnityEngineDebug.__Log__SystemObject__SystemVoid msg

  # 間接 goto ステートメント
  #
  #   goto_indirect SystemUInt32型の変数
  #
  # 変数に入ってるアドレスに向かって飛ぶ
  goto_indirect returnAddr

# ラベル定義も変数定義と同様に，pub を付けると出力に .export ラベル名 が追記される
# ラベルには sync はない
label pub @_start
  let x : SystemBoolean = null

  # このようにラベルのアドレスを SystemUInt32 の変数に突っ込むことができる
  let nextAddr : SystemUInt32 = @next

  # 条件付き goto ステートメント
  #
  #   goto_if_false SystemBoolean型の変数 ラベル
  #
  # 変数が false (null) のときにラベルに飛ぶ
  goto_if_false x @foo

  goto @next
  label @foo

  # 代入ステートメント
  #
  #   set コピー先の変数 コピー元の変数
  #
  # コピー元の変数の値をコピー先の変数にコピーする
  set returnAddr nextAddr

  goto @func
  label @next

  # exit ステートメント
  # プログラムを終了
  exit

# このほかに UAssembly と同じ push 変数, pop, copy ステートメントがある
# また call で引数の変数を一個も与えなければ UAssembly の extern と同じ動きになる
# UAssembly のスーパーセットにするために入れてる
```

## ライブラリとして

こいつの本質は

``` fsharp
module Ast =
  type Literal<'label> =
    | This
    | Null
    | IntLiteral of int
    | StringLiteral of string
    | FloatLiteral of float
    | Label of 'label

  type Expr<'Label> =
    | Var of name:string
  type ExprWithInfo<'Label, 'Info> = With<Expr<'Label>, 'Info>

  [<RequireQualifiedAccess>]
  type VariableSyncType =
    | None
    | Itself of interpolationAlgorithmName:string
    | Property of prop:string * interpolationAlgorithmName:string

  type Stmt<'Label, 'Info> =
    | DefineVar of ty:string * var:string * isPublic:bool * sync:VariableSyncType * With<Literal<'Label>, 'Info>
    | DefineLabel of name:string * isPublic:bool
    | Call of funcName:string * args:ExprWithInfo<'Label, 'Info> list
    | Assign of var:string * ExprWithInfo<'Label, 'Info>
    | Goto of label:string
    | GotoIfFalse of cond:ExprWithInfo<'Label, 'Info> * label:string
    | GotoIndirect of ExprWithInfo<'Label, 'Info>
    | Push of arg:ExprWithInfo<'Label, 'Info>
    | Pop
    | Copy
    | Exit
    | Comment of string
  and StmtWithInfo<'Label, 'Info> = With<Stmt<'Label, 'Info>, 'Info>

  type Program<'info> = StmtWithInfo<string, 'info> list
```

の `Program<'info>` を

```fsharp
module Compiler =
  type [<Measure>] addr

  type VarTable<'Label> = Map<string, int<addr> * string * bool * Ast.VariableSyncType * Ast.Literal<'Label>>
  type LabelTable = Map<string, int<addr> * bool>

  type Op =
    | Nop
    | Push of int<addr>
    | Pop
    | Jump of int<addr>
    | JumpIf of int<addr>
    | JumpIndirect of int<addr>
    | Extern of string
    | Copy
    | Label of string

  type Assembly = VarTable<int<addr>> * LabelTable * Op list
```

の `Assembly` に変換するライブラリであることです

UAssembly のアセンブラ言語も，UdonVM のバイナリも吐けます（ヒープの扱いが面倒ですが）

`Program<'info>` に手書き用の構文を定義して，パーサと Web のガワを付けてリリースしたのがこれです


