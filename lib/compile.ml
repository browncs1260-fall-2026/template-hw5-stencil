open Asm.Directive
open Ast
open Util

(** constants used for tagging values at runtime *)
let num_shift = 2

let num_mask = 0b11

let num_tag = 0b00

let bool_shift = 7

let bool_mask = 0b1111111

let bool_tag = 0b0011111

let heap_mask = 0b111

let pair_tag = 0b010

let nil_tag = 0b11111111

type symtab = int Symtab.symtab

(** [operand_of_num x] returns the runtime representation of the number [x] as an
   operand for instructions *)
let operand_of_num (x : int) : operand = Imm ((x lsl num_shift) lor num_tag)

let operand_of_nil = Imm nil_tag

(** [operand_of_bool b] returns the runtime representation of the boolean [b] as an
   operand for instructions *)
let operand_of_bool (b : bool) : operand =
  Imm (((if b then 1 else 0) lsl bool_shift) lor bool_tag)

let zf_to_bool =
  [ Mov (Reg Rax, Imm 0)
  ; Setz (Reg Rax)
  ; Shl (Reg Rax, Imm bool_shift)
  ; Or (Reg Rax, Imm bool_tag) ]

let setl_bool =
  [ Mov (Reg Rax, Imm 0)
  ; Setl (Reg Rax)
  ; Shl (Reg Rax, Imm bool_shift)
  ; Or (Reg Rax, Imm bool_tag) ]

let stack_address (index : int) : operand = MemOffset (Imm index, Reg Rsp)

let ensure condition err_msg =
  let msg_label = gensym "emsg" in
  let continue_label = gensym "continue" in
  condition
  @ [ Je continue_label
    ; LeaLabel (Reg Rdi, msg_label)
    ; Jmp "lisp_error"
    ; Label msg_label
    ; DqString err_msg
    ; Label continue_label ]

let ensure_type mask tag op e =
  ensure
    [Mov (Reg R8, op); And (Reg R8, Imm mask); Cmp (Reg R8, Imm tag)]
    (string_of_expr e)

let ensure_num = ensure_type num_mask num_tag

let ensure_pair = ensure_type heap_mask pair_tag

let align_stack_index (stack_index : int) : int =
  if stack_index mod 16 = -8 then stack_index else stack_index - 8

(** [compile_0ary_primitive e prim] produces X86-64 instructions for the zero-arity
   primitive operation  [prim]; if [prim] isn't a valid zero-arity operation,
   it raises an error using the expression [e] *)
let compile_0ary_primitive stack_index _e = function
  | ReadNum ->
      [ Mov (stack_address stack_index, Reg Rdi)
      ; Add (Reg Rsp, Imm (align_stack_index stack_index))
      ; Call "read_num"
      ; Sub (Reg Rsp, Imm (align_stack_index stack_index))
      ; Mov (Reg Rdi, stack_address stack_index) ]
  | Newline ->
      [ Mov (stack_address stack_index, Reg Rdi)
      ; Add (Reg Rsp, Imm (align_stack_index stack_index))
      ; Call "print_newline"
      ; Sub (Reg Rsp, Imm (align_stack_index stack_index))
      ; Mov (Reg Rdi, stack_address stack_index)
      ; Mov (Reg Rax, operand_of_bool true) ]

(** [compile_unary_primitive e prim] produces X86-64 instructions for the unary
   primitive operation [prim]; if [prim] isn't a valid unary operation,
   it raises an error using the expression [e] *)
let compile_unary_primitive stack_index e = function
  | Add1 ->
      ensure_num (Reg Rax) e @ [Add (Reg Rax, operand_of_num 1)]
  | Sub1 ->
      ensure_num (Reg Rax) e @ [Sub (Reg Rax, operand_of_num 1)]
  | IsZero ->
      [Cmp (Reg Rax, operand_of_num 0)] @ zf_to_bool
  | IsNum ->
      [And (Reg Rax, Imm num_mask); Cmp (Reg Rax, Imm num_tag)] @ zf_to_bool
  | Not ->
      [Cmp (Reg Rax, operand_of_bool false)] @ zf_to_bool
  | IsPair ->
      [And (Reg Rax, Imm heap_mask); Cmp (Reg Rax, Imm pair_tag)] @ zf_to_bool
  | Left ->
      ensure_pair (Reg Rax) e
      @ [Mov (Reg Rax, MemOffset (Reg Rax, Imm (-pair_tag)))]
  | Right ->
      ensure_pair (Reg Rax) e
      @ [Mov (Reg Rax, MemOffset (Reg Rax, Imm (-pair_tag + 8)))]
  | Print ->
      [ Mov (stack_address stack_index, Reg Rdi)
      ; Mov (Reg Rdi, Reg Rax)
      ; Add (Reg Rsp, Imm (align_stack_index stack_index))
      ; Call "print_value"
      ; Sub (Reg Rsp, Imm (align_stack_index stack_index))
      ; Mov (Reg Rdi, stack_address stack_index)
      ; Mov (Reg Rax, operand_of_bool true) ]

(** [compile_binary_primitive stack_index e prim] produces X86-64 instructions
   for the binary primitive operation [prim]; if [prim] isn't a valid
   binary operation, it raises an error using the expression [e] *)
let compile_binary_primitive stack_index e = function
  | Plus ->
      ensure_num (Reg Rax) e
      @ ensure_num (stack_address stack_index) e
      @ [Add (Reg Rax, stack_address stack_index)]
  | Minus ->
      ensure_num (Reg Rax) e
      @ ensure_num (stack_address stack_index) e
      @ [ Mov (Reg R8, Reg Rax)
        ; Mov (Reg Rax, stack_address stack_index)
        ; Sub (Reg Rax, Reg R8) ]
  | Eq ->
      ensure_num (Reg Rax) e
      @ ensure_num (stack_address stack_index) e
      @ [Cmp (stack_address stack_index, Reg Rax)]
      @ zf_to_bool
  | Lt ->
      ensure_num (Reg Rax) e
      @ ensure_num (stack_address stack_index) e
      @ [Cmp (stack_address stack_index, Reg Rax)]
      @ setl_bool
  | Pair ->
      [ Mov (Reg R8, stack_address stack_index)
      ; Mov (MemOffset (Reg Rdi, Imm 0), Reg R8)
      ; Mov (MemOffset (Reg Rdi, Imm 8), Reg Rax)
      ; Mov (Reg Rax, Reg Rdi)
      ; Or (Reg Rax, Imm pair_tag)
      ; Add (Reg Rdi, Imm 16) ]

let align n alignment =
  if n mod alignment = 0 then n else n + (alignment - (n mod alignment))

(** [compile_expr e] produces X86-64 instructions for the expression [e] *)
let rec compile_expr (tab : symtab) (stack_index : int) : expr -> directive list
    = function
  | Num x ->
      [Mov (Reg Rax, operand_of_num x)]
  | Nil ->
      [Mov (Reg Rax, operand_of_nil)]
  | True ->
      [Mov (Reg Rax, operand_of_bool true)]
  | False ->
      [Mov (Reg Rax, operand_of_bool false)]
  | Var var when Symtab.mem var tab ->
      [Mov (Reg Rax, stack_address (Symtab.find var tab))]
  | Var _ as e ->
      raise (Error.Stuck e)
  | If (test_expr, then_expr, else_expr) ->
      let then_label = gensym "then" in
      let else_label = gensym "else" in
      let continue_label = gensym "continue" in
      compile_expr tab stack_index test_expr
      @ [Cmp (Reg Rax, operand_of_bool false); Je else_label]
      @ [Label then_label]
      @ compile_expr tab stack_index then_expr
      @ [Jmp continue_label] @ [Label else_label]
      @ compile_expr tab stack_index else_expr
      @ [Label continue_label]
  | Let (bindings, body) ->
      let compiled =
        bindings
        |> List.mapi (fun i (_, exp) ->
               compile_expr tab (stack_index - (8 * i)) exp
               @ [Mov (stack_address (stack_index - (8 * i)), Reg Rax)] )
      in
      let new_tab =
        bindings
        |> List.mapi (fun i (v, _) -> (v, stack_index - (8 * i)))
        |> List.fold_left (fun new_tab (v, i) -> Symtab.add v i new_tab) tab
      in
      List.concat compiled
      @ compile_expr new_tab (stack_index - (8 * List.length bindings)) body
  | Do exps ->
      List.concat_map (compile_expr tab stack_index) exps
  | Prim0 f as exp ->
      compile_0ary_primitive stack_index exp f
  | Prim1 (f, arg) as exp ->
      compile_expr tab stack_index arg
      @ compile_unary_primitive stack_index exp f
  | Prim2 (f, arg1, arg2) as exp ->
      compile_expr tab stack_index arg1
      @ [Mov (stack_address stack_index, Reg Rax)]
      @ compile_expr tab (stack_index - 8) arg2
      @ compile_binary_primitive stack_index exp f

(** [compile] produces X86-64 instructions, including frontmatter, for the
   expression [e] *)
let compile = function
  | e :: _ ->
      [ Global "lisp_entry"
      ; Extern "lisp_error"
      ; Extern "read_num"
      ; Extern "print_value"
      ; Extern "print_newline"
      ; Extern "open_in"
      ; Extern "open_out"
      ; Extern "close_channel"
      ; Extern "input"
      ; Extern "output"
      ; Section "text"
      ; Label "lisp_entry" ]
      @ compile_expr Symtab.empty (-8) (Lisp_syntax.expr_of_s_exp e)
      @ [Ret]
  | [] ->
      failwith "Empty source file"
