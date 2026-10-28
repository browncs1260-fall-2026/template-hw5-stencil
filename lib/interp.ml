open Printf
open Util
open Ast

(** a [value]  is the runtime value of an expression *)
type value =
  | Num of int
  | Bool of bool
  | Pair of (value * value)
  | Nil

type environment = value Symtab.symtab

(** [display_value v] returns a string representation of the runtime value [v] *)
let rec display_value = function
  | Num x ->
      sprintf "%d" x
  | Bool b ->
      if b then "true" else "false"
  | Pair (v1, v2) ->
      sprintf "(pair %s %s)" (display_value v1) (display_value v2)
  | Nil ->
      "()"

let interp_0ary_primitive prim =
  match prim with
  | ReadNum ->
      Some (Num (input_line stdin |> String.trim |> int_of_string))
  | Newline ->
      print_string "\n" ; Some (Bool true)

let interp_unary_primitive prim arg =
  match (prim, arg) with
  | Add1, Num x ->
      Some (Num (x + 1))
  | Sub1, Num x ->
      Some (Num (x - 1))
  | IsZero, Num 0 ->
      Some (Bool true)
  | IsZero, _ ->
      Some (Bool false)
  | IsNum, Num _ ->
      Some (Bool true)
  | IsNum, _ ->
      Some (Bool false)
  | Not, Bool false ->
      Some (Bool true)
  | Not, _ ->
      Some (Bool false)
  | IsPair, Pair _ ->
      Some (Bool true)
  | IsPair, _ ->
      Some (Bool false)
  | Left, Pair (v, _) ->
      Some v
  | Right, Pair (_, v) ->
      Some v
  | Print, v ->
      v |> display_value |> print_string ;
      Some (Bool true)
  | _ ->
      None

let interp_binary_primitive prim arg1 arg2 =
  match (prim, arg1, arg2) with
  | Plus, Num x1, Num x2 ->
      Some (Num (x1 + x2))
  | Minus, Num x1, Num x2 ->
      Some (Num (x1 - x2))
  | Eq, Num x1, Num x2 ->
      Some (Bool (x1 = x2))
  | Lt, Num x1, Num x2 ->
      Some (Bool (x1 < x2))
  | Pair, v1, v2 ->
      Some (Pair (v1, v2))
  | _ ->
      None

let rec interp_expr (env : environment) : expr -> value = function
  | Num x ->
      Num x
  | Var var as e -> (
    match Symtab.find_opt var env with
    | Some value ->
        value
    | None ->
        raise (Error.Stuck e) )
  | Prim0 f as e -> (
    match interp_0ary_primitive f with
    | Some v ->
        v
    | None ->
        raise (Error.Stuck e) )
  | Prim1 (f, arg) as e -> (
    match interp_unary_primitive f (interp_expr env arg) with
    | Some v ->
        v
    | None ->
        raise (Error.Stuck e) )
  | Prim2 (f, arg1, arg2) as e -> (
    match
      let v1 = interp_expr env arg1 in
      let v2 = interp_expr env arg2 in
      interp_binary_primitive f v1 v2
    with
    | Some v ->
        v
    | None ->
        raise (Error.Stuck e) )
  | Let (bindings, body) ->
      let new_env =
        List.fold_left
          (fun new_env (var, exp) ->
            Symtab.add var (interp_expr env exp) new_env )
          env bindings
      in
      interp_expr new_env body
  | If (test_exp, then_exp, else_exp) ->
      if interp_expr env test_exp <> Bool false then interp_expr env then_exp
      else interp_expr env else_exp
  | Do exps ->
      exps |> List.rev_map (interp_expr env) |> List.hd
  | True ->
      Bool true
  | False ->
      Bool false
  | Nil ->
      Nil

let interp = function
  | e :: _ ->
      Lisp_syntax.expr_of_s_exp e
      |> interp_expr Symtab.empty |> display_value |> ignore
  | _ ->
      failwith "Empty source file"
