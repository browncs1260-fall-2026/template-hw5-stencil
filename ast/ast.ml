open S_exp

type prim0 = ReadNum | Newline

type prim1 =
  | Add1
  | Sub1
  | IsZero
  | IsNum
  | Not
  | IsPair
  | Left
  | Right
  | Print
  | OpenIn
  | OpenOut
  | CloseIn
  | CloseOut

type prim2 = Plus | Minus | Eq | Lt | Pair | Input | Output

type expr =
  | Num of int
  | Var of string
  | String of string
  | True
  | False
  | Nil
  | Stdin
  | Stdout
  | Prim0 of prim0
  | Prim1 of prim1 * expr
  | Prim2 of prim2 * expr * expr
  | If of expr * expr * expr
  | Let of (string * expr) list * expr
  | Do of expr list

(***** AST to S_exp *****)
let rec s_exp_of_expr = function
  | True ->
      Sym "true"
  | False ->
      Sym "false"
  | Nil ->
      Lst []
  | Stdin ->
      Sym "stdin"
  | Stdout ->
      Sym "stdout"
  | Num n ->
      Num n
  | Var var ->
      Sym var
  | String s ->
      Str s
  | Prim0 ReadNum ->
      Lst [Sym "read-num"]
  | Prim0 Newline ->
      Lst [Sym "newline"]
  | Prim1 (Add1, arg) ->
      Lst [Sym "add1"; s_exp_of_expr arg]
  | Prim1 (Sub1, arg) ->
      Lst [Sym "sub1"; s_exp_of_expr arg]
  | Prim1 (IsZero, arg) ->
      Lst [Sym "zero?"; s_exp_of_expr arg]
  | Prim1 (IsNum, arg) ->
      Lst [Sym "num?"; s_exp_of_expr arg]
  | Prim1 (Not, arg) ->
      Lst [Sym "not"; s_exp_of_expr arg]
  | Prim1 (IsPair, arg) ->
      Lst [Sym "pair?"; s_exp_of_expr arg]
  | Prim1 (Left, arg) ->
      Lst [Sym "left"; s_exp_of_expr arg]
  | Prim1 (Right, arg) ->
      Lst [Sym "right"; s_exp_of_expr arg]
  | Prim1 (Print, arg) ->
      Lst [Sym "print"; s_exp_of_expr arg]
  | Prim1 (OpenIn, arg) ->
      Lst [Sym "open-in"; s_exp_of_expr arg]
  | Prim1 (OpenOut, arg) ->
      Lst [Sym "open-out"; s_exp_of_expr arg]
  | Prim1 (CloseIn, arg) ->
      Lst [Sym "close-in"; s_exp_of_expr arg]
  | Prim1 (CloseOut, arg) ->
      Lst [Sym "close-out"; s_exp_of_expr arg]
  | Prim2 (Plus, arg1, arg2) ->
      Lst [Sym "+"; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | Prim2 (Minus, arg1, arg2) ->
      Lst [Sym "-"; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | Prim2 (Lt, arg1, arg2) ->
      Lst [Sym "<"; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | Prim2 (Eq, arg1, arg2) ->
      Lst [Sym "="; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | Prim2 (Pair, arg1, arg2) ->
      Lst [Sym "pair"; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | Prim2 (Input, arg1, arg2) ->
      Lst [Sym "input"; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | Prim2 (Output, arg1, arg2) ->
      Lst [Sym "output"; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | If (e1, e2, e3) ->
      Lst [Sym "if"; s_exp_of_expr e1; s_exp_of_expr e2; s_exp_of_expr e3]
  | Let (exps, eval_exp) ->
      let bindings =
        Lst
          (List.map
             (fun (e : string * expr) ->
               match e with v, expr -> Lst [Sym v; s_exp_of_expr expr] )
             exps )
      in
      Lst [Sym "let"; bindings; s_exp_of_expr eval_exp]
  | Do exps ->
      Lst (Sym "do" :: List.map s_exp_of_expr exps)

let string_of_expr s = s |> s_exp_of_expr |> string_of_s_exp
