module Sanuki

module Ast =
  type Literal<'label> =
    | This
    | Null
    | IntLiteral of int
    | StringLiteral of string
    | FloatLiteral of float
    | Label of 'label
    static member Map (x, f) =
      match x with
      | This -> This | Null -> Null
      | IntLiteral i -> IntLiteral i
      | StringLiteral s -> StringLiteral s
      | FloatLiteral f -> FloatLiteral f
      | Label l -> Label (f l)

  type With<'T, 'U> =
    { item: 'T; info: 'U }
    override x.ToString() = x.item.ToString()

  module With =
    let inline itemof x = x.item
    let inline infoof x = x.info
    let inline bind f x : With<_, _> = f x.item
    let inline map f x = { item = f x.item; info = x.info }
    let inline mapInfo f x = { item = x.item; info = f x.info }
    let inline sameInfoOf orig x = { item = x; info = orig.info }
    let inline info i x = { item = x; info = i }
    let inline noinfo x = { item = x; info = Unchecked.defaultof<_> }
   
  let (|With|) x = With (x.item, x.info)
  let (|Item|) x = Item x.item
  let (|Info|) x = Info x.info

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
    | GotoIf of cond:ExprWithInfo<'Label, 'Info> * label:string
    | GotoIndirect of ExprWithInfo<'Label, 'Info>
    | Push of arg:ExprWithInfo<'Label, 'Info>
    | Pop
    | Copy
    | Exit
  and StmtWithInfo<'Label, 'Info> = With<Stmt<'Label, 'Info>, 'Info>

  type Range = { StartPos: Parsec.Position; EndPos: Parsec.Position }

  type Program<'info> = StmtWithInfo<string, 'info> list
  type ParsedProgram = Program<Range>

module Parser =
  open Parsec
  open Parsec.Extensions
  open Ast

  let inline syn x = skipString x .>> spaces
  let inline syn1 x = skipString x .>> spaces1
  let inline cyn x = skipChar x .>> spaces
  let inline cyn1 x = skipChar x .>> spaces1
  
  let inline withPos (p: Parser<'a, _>) : Parser<With<'a, Position>, _> =
    fun (state, stream) ->
      let pos = stream.pos
      match p (state, stream) with
      | Ok (v, s, state) -> Ok (v |> With.info pos, s, state)
      | Error e -> Error e

  let inline withRange (p: Parser<'a, _>) : Parser<With<'a, Range>, _> =
    fun (state, stream) ->
      let startPos = stream.pos
      match p (state, stream) with
      | Ok (v, s, state) -> Ok (v |> With.info { StartPos = startPos; EndPos = s.pos }, s, state)
      | Error e -> Error e

  let reserved =
    Set.ofList [
      "let"; "label"; "pub"; "sync"; "call"; "set"; "exit" // basic keyword
      "goto"; "goto_if"; "goto_indirect" // goto-like
      "pop"; "push"; "copy" // unsafe
      "this"; "null" // literal
    ]
  
  let inline excludeReserved (i: Parser<string, unit>) : Parser<string, unit> =
    fun (state, stream) ->
      match i (state, stream) with
      | Error e -> Error e
      | Ok (v, s, state) as res ->
        if reserved |> Set.contains v then
          Error ([s.pos, [Message (sprintf "identifier '%s' is reserved." v)]], state)
        else res
  let pVarName =
    many1Satisfy2L
      isAsciiLetter
      (fun c -> isAsciiLetter c || isDigit c || c = '_')
      "<variable>"
    |> excludeReserved
  let pLabelName =
    many1Satisfy2L
      (fun c -> isAsciiLetter c || c = '_')
      (fun c -> isAsciiLetter c || isDigit c || c = '_')
      "<identifier>"
    |> excludeReserved

  let pIntLiteral = pint32 |>> IntLiteral
  let pFloatLiteral = pfloat |>> FloatLiteral
  let pStringLiteral = between (skipString "\"") (skipString "\"") (escapedString "\"") |>> StringLiteral
  let pThisLiteral = stringReturn "this" This
  let pNullLiteral = stringReturn "null" Null
  let pLabel = skipChar '@' >>. pLabelName <?> "@<label>"
  let pLabelLiteral = pLabel |>> Label
  let pLiteral =
    pFloatLiteral <|> pIntLiteral <|> pStringLiteral
                  <|> pThisLiteral  <|> pNullLiteral <|> pLabelLiteral
  let pVariableExpr = pVarName |>> Var |> withRange
  let pExpr = pVariableExpr <?> "<variable>"

  let pSync : Parser<VariableSyncType> =
    syn "sync" >>. choice [
      between (cyn '[') (cyn ']') pLabelName
             .>>. between (cyn '<') (cyn '>') pLabelName |>> VariableSyncType.Property 
      between (cyn '<') (cyn '>') pLabelName |>> VariableSyncType.Itself
    ]

  let pDefVarStmt =
    pipe3
      (syn "let" >>. (opt (syn "pub")) .>>. (opt pSync |>> Option.defaultValue VariableSyncType.None) .>>. ws pVarName)
      (syn ":" >>. ws pVarName)
      (syn "=" >>. withRange pLiteral)
      (fun ((p, s), n) ty v -> DefineVar(ty, n, p.IsSome, s, v))
    |> withRange
    |> ws
  let pDefLabelStmt =
    syn "label" >>. (opt (syn "pub")) .>>. pLabel
    |>> (fun (b, l) -> DefineLabel (l, b.IsSome))
    |> withRange
    |> ws
  let pExternName = pVarName .>> skipChar '.' .>>. pLabelName |>> fun (s, t) -> sprintf "%s.%s" s t
  let pCallStmt =
    syn "call" >>. ws pExternName .>>. sepEndBy pExpr spaces1 |>> Call |> withRange
  let pAssignStmt =
    syn "set" >>. ws pVarName .>>. pExpr |>> Assign |> withRange |> ws
  let pGotoStmts =
    skipString "goto" >>. choice [
      syn "_if" >>. ws pExpr .>>. pLabel |>> GotoIf
      syn "_indirect" >>. pExpr |>> GotoIndirect
      spaces >>. pLabel |>> Goto
    ] |> withRange |> ws
  let pPushStmt = syn "push" >>. pExpr |>> Push |> withRange |> ws
  let pPopStmt  = skipString "pop"  >>% Pop |> withRange |> ws
  let pCopyStmt = skipString "copy"  >>% Copy |> withRange |> ws
  let pExitStmt = skipString "exit" >>% Exit |> withRange |> ws

  let pStmt =
    pDefVarStmt <|> pDefLabelStmt <|> pCallStmt <|> pAssignStmt
                <|> pGotoStmts <|> pPushStmt <|> pPopStmt <|> pCopyStmt <|> pExitStmt

  let pProgram : Parser<ParsedProgram, _> = spaces >>. manyTill pStmt eof
  
  let parseString str = runString pProgram () str

let src = """
let pub sync[Value]<SomeMethod> foo:SystemInt32 = 0
let sync[Value]<SomeMethod> bar:SystemInt32 = 0
let sync<SomeMethod> baz:SystemInt32 = 0
let returnAddr:SystemUInt32=0

label @func
  let msg:SystemString = "\"Hello, World!\n\""
  call Debug.Log msg
  goto_indirect returnAddr

label pub @_start
  let pub x : SystemBool = null
  goto_if x @next
  let nextAddr:SystemUInt32 = @next
  set returnAddr nextAddr
  goto @func
  label @next
  push x
  push baz
  copy
  push x
  pop
  exit
"""

module Compiler =
  type [<Measure>] addr

  type VarTable<'Label> = Map<string, int<addr> * string * bool * Ast.VariableSyncType * Ast.Literal<'Label>>
  type LabelTable = Map<string, int<addr> * bool>

  type CompileResult<'a, 'warn, 'error> =
    | COk of 'a * 'warn list
    | CError of 'warn list * 'error list
    static member inline Return x = COk (x, [])
    static member inline ( >>= ) (x: CompileResult<'a, 'w, 'e>, f: 'a -> CompileResult<'b, 'w, 'e>) : CompileResult<'b, 'w, 'e> =
      match x with
      | CError (ws, es) -> CError (ws, es)
      | COk (x, ws) ->
        match f x with
        | COk (y, ws') -> COk (y, ws @ ws')
        | CError (ws', es) -> CError (ws @ ws', es)
    static member inline Map (x: CompileResult<_, _, _>, f) = x >>= fun y -> COk (f y, [])
  module CompileResult =
    let inline result x = COk (x, [])
    let inline returnError e = CError ([], [e])
    let inline addWarn w x =
      match x with COk (x, ws) -> COk (x, w :: ws) | CError (ws, es) -> CError (w :: ws, es)
    let inline mapWarn f x =
      match x with COk (x, ws) -> COk (x, List.map f ws) | CError (ws, es) -> CError (List.map f ws, es)
    let inline mapError f x =
      match x with COk (x, ws) -> COk (x, ws) | CError (ws, es) -> CError (ws, List.map f es)
  open CompileResult

  let inline map f (x: ^X) = (^X: (static member Map: _*_->_) x,f)

  type CompilerMsg<'info> = CompilerMsg of 'info * msg:string with
    static member inline Map (CompilerMsg (i, m), f) = CompilerMsg (f i, m)

  type CompileMsgResult<'a, 'info> =
    CompileResult<'a, CompilerMsg<'info>, CompilerMsg<'info>>
  
  [<RequireQualifiedAccess>]
  type Op =
    | Nop
    | Push of int<addr>
    | Pop
    | Jump of int<addr>
    | JumpIf of int<addr>
    | JumpIndirect of int<addr>
    | Extern of string
    | Copy

  type Assembly = VarTable<int<addr>> * Op list

  type AbstractOp =
    | Nop
    | Push of var:string
    | Pop
    | Label of name:string * isPublic:bool
    | Jump of label:string
    | JumpIf of label:string
    | JumpIndirect of var:string
    | Extern of string
    | Copy
    | Exit
  type AbstractOps<'info> = Ast.With<AbstractOp, 'info> list

  module Program =
    open Ast

    // type Stmt<'Label, 'Info> =
    //   | DefineVar of ty:string * var:string * isPublic:bool * Literal<'Label>
    //   | DefineLabel of name:string * isPublic:bool
    //   | Call of funcName:string * args:ExprWithInfo<'Label, 'Info> list
    //   | Assign of var:string * ExprWithInfo<'Label, 'Info>
    //   | Goto of label:string
    //   | GotoIf of cond:ExprWithInfo<'Label, 'Info> * label:string
    //   | GotoIndirect of ExprWithInfo<'Label, 'Info>
    //   | Exit
    let toAbstractOp (externArity: Map<string, int>) (p: Program<'info>) : CompileMsgResult<VarTable<string> * AbstractOps<'info>, 'info> =
      let gensym =
        let c = ref 0<addr>
        fun () ->
          let v = !c
          c := v + 1<addr>
          v
     
      let checkLiteralType ty l =
        match ty, l with
        | "SystemString", StringLiteral _
        | "SystemUInt32", Label _
        | ("SystemInt32" | "SystemUInt32" | "SystemSingle" | "SystemDouble"), IntLiteral _
        | ("SystemSingle" | "SystemDouble"), FloatLiteral _
        | _, (This | Null) -> true
        | _ -> false

      let rec go (ops: Program<_>) (vt: VarTable<string>, lt: Set<string>, acc) =
        match ops with
        | [] -> result (vt, acc)
        | With (x, pos) :: rest ->
          let result =
            match x with
            | DefineVar (ty, var, p, s, With(l, lPos)) ->
              if vt |> Map.containsKey var then
                CompilerMsg (pos, sprintf "variable %s is defined twice" var) |> returnError
              else if checkLiteralType ty l then
                result (vt |> Map.add var (gensym(), ty, p, s, l), lt, [])
              else
                CompilerMsg (lPos, sprintf "this literal cannot be used to type %s." ty) |> returnError
            | DefineLabel (n, p) ->
              if lt |> Set.contains n |> not then
                result (vt, lt |> Set.add n, [AbstractOp.Label (n, p)])
              else
                CompilerMsg (pos, sprintf "label %s is defined twice" n) |> returnError
            | Call (fn, args) ->
              let pushes =
                List.foldBack (fun x state ->
                  state |> Result.bind (fun state ->
                    match x.item with
                    | Var v ->
                      if vt |> Map.containsKey v then
                        Ok (AbstractOp.Push v :: state)
                      else
                        Error (x.info, v)
                  )) args (Ok [])
              match pushes with
              | Ok pushes ->
                let ops = [
                  yield! pushes 
                  yield AbstractOp.Extern fn
                ]
                let res = result (vt, lt, ops)
                match externArity |> Map.tryFind fn with
                | None -> res
                | Some i ->
                  let len = List.length args
                  if len = i then res
                  else
                    res |> addWarn (CompilerMsg (pos, sprintf "extern %s has an arity %i but given only %i arg(s). this will only push the last %i arg(s)." fn i len len))
              | Error (info, v) -> CompilerMsg (info, sprintf "variable %s is not defined" v) |> returnError
            | Assign (v, With(Var u, ePos)) ->
              match Map.tryFind v vt, Map.tryFind u vt with
              | Some (_, vTy, _, _, _), Some (_, uTy, _, _, _) ->
                if vTy = uTy then
                  result (vt, lt, [AbstractOp.Push u; AbstractOp.Push v; AbstractOp.Copy])
                else
                  CompilerMsg (pos, sprintf "type mismatch. %s:%s != %s:%s." v vTy u uTy) |> returnError
              | Some _, None -> CompilerMsg (ePos, sprintf "variable %s is not defined" u) |> returnError
              | None, _ -> CompilerMsg (pos, sprintf "variable %s is not defined" v) |> returnError
            | Goto l -> result (vt, lt, [AbstractOp.Jump l])
            | GotoIf (With (Var v, ePos), l) ->
              match Map.tryFind v vt with
              | Some (_, "SystemBool", _, _, _) ->
                result (vt, lt, [AbstractOp.Push v; AbstractOp.JumpIf l])
              | Some (_, ty, _, _, _) ->
                CompilerMsg (ePos, sprintf "type %s is not SystemBoolean." ty) |> returnError
              | None -> CompilerMsg (pos, sprintf "variable %s is not defined" v) |> returnError
            | GotoIndirect (With (Var v, ePos)) ->
              match Map.tryFind v vt with
              | Some (_, "SystemUInt32", _, _, _) ->
                result (vt, lt, [AbstractOp.JumpIndirect v])
              | Some (_, ty, _, _, _) ->
                CompilerMsg (ePos, sprintf "type %s is not SystemUInt32." ty) |> returnError
              | None -> CompilerMsg (pos, sprintf "variable %s is not defined" v) |> returnError
            | Push (With (Var v, ePos)) ->
              if vt |> Map.containsKey v then
                result (vt, lt, [AbstractOp.Push v]) |> addWarn (CompilerMsg (pos, "push is unsafe. use it with care."))
              else
                CompilerMsg (ePos, sprintf "variable %s is not defined" v) |> returnError
            | Pop    -> result (vt, lt, [AbstractOp.Pop]) |> addWarn (CompilerMsg (pos, "pop is unsafe. use it with care."))
            | Copy   -> result (vt, lt, [AbstractOp.Copy]) |> addWarn (CompilerMsg (pos, "copy is unsafe. use it with care."))
            | Exit -> result (vt, lt, [AbstractOp.Exit])
          result >>= fun (vt, lt, ops) -> go rest (vt, lt, acc @ (ops |> List.map (With.info pos)))
      go p (Map.empty, Set.empty, [])

  module AbstractOp =
    let length (op: AbstractOp) =
      match op with
      | Label _ -> 0<addr>
      | Nop | Pop | Copy -> 1<addr>
      | Push _ | Jump _ | JumpIf _| JumpIndirect _ | Extern _ | Exit -> 5<addr>

    let getLabelAddress info label ops =
      let rec go len ops =
        match ops with
        | Ast.Item (Label (l, _)) :: _ when l = label -> result len
        | Ast.Item op :: ops -> go (len + length op) ops
        | [] -> CompilerMsg(info, "label '%s' is not defined") |> returnError
      go 0<addr> ops

    let createLabelTable (ops: AbstractOps<_>) : CompileMsgResult<LabelTable, _> =
      ops |> List.choose (function Ast.With(Label (l, p), info) -> Some ((l, p), info) | _ -> None)
          |> List.groupBy fst
          |> List.map (function ((l, p), [(_,info)]) -> getLabelAddress info l ops |> map (fun x -> l,(x,p))
                              | _, [] -> failwith "impossible"
                              | (l, _), xs ->
                                CompilerMsg((List.last xs |> snd), sprintf "label %s is defined twice" l)
                                |> returnError)
          |> List.fold (fun state x ->
              state >>= fun t ->
                x >>= fun (k, v) ->
                  t |> Map.add k v |> result
            ) (result Map.empty)
          
    let replaceLabelsInVarTable (labelTable: LabelTable) (varTable: VarTable<string>) : VarTable<int<addr>> =
      varTable |> Map.map (fun _ (addr, ty, pub, sync, l) ->
        (addr, ty, pub, sync, l |> map (fun x -> labelTable |> Map.find x |> fst)))

    let inline item1 (x,_,_,_,_) = x
    let toOp (labelTable: LabelTable) (varTable: VarTable<int<addr>>) (ops: AbstractOps<_>) : Op list =
      let rec f = function
        | [] -> []
        | Nop :: xs -> Op.Nop :: f xs
        | Push var :: xs -> Op.Push (varTable |> Map.find var |> item1) :: f xs
        | Pop :: xs -> Op.Pop :: f xs
        | Label _ :: xs -> f xs
        | Jump label :: xs -> Op.Jump (labelTable |> Map.find label |> fst) :: f xs
        | JumpIf label :: xs -> Op.JumpIf (labelTable |> Map.find label |> fst) :: f xs
        | JumpIndirect var :: xs -> Op.JumpIndirect (varTable |> Map.find var |> item1) :: f xs
        | Extern s :: xs -> Op.Extern s :: f xs
        | Copy :: xs -> Op.Copy :: f xs
        | Exit :: xs -> Op.Jump 0xFFFFFF<addr> :: f xs
      f (ops |> List.map (fun x -> x.item))

  let compile (p: Ast.Program<'a>) : CompileMsgResult<Assembly, 'a> =
    Program.toAbstractOp Map.empty p 
    >>= fun (vt, aops) ->
      AbstractOp.createLabelTable aops
      >>= fun lt ->
        let vt = AbstractOp.replaceLabelsInVarTable lt vt
        (vt, AbstractOp.toOp lt vt aops) |> result

open Compiler

let test str : CompileMsgResult<Assembly, Ast.Range>=
  match str |> Parser.parseString with
  | Ok (x, _, _) -> x |> compile
  | Error ((es, _) as e) ->
    Parsec.ParseError.prettyPrint e (Some str) |> printfn "%s"
    CError ([], [for pos, ms in es do for m in ms do yield CompilerMsg ({ StartPos = pos; EndPos = pos }, sprintf "%A" m)])
