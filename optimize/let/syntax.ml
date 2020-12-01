(* Syntax.t: parser が出力する抽象構文木の型 *)

type t = Number of int
       | Real of float
       | Variable of string
       | Op of t * Operator.t * t
       | IfEqual of t * t * t * t
       | IfLess of t * t * t * t
       | Let of (string * Type.t) * t * t
       | LetRec of (string * Type.t) * (string * Type.t) list * t * t
       | Application of t * t list

(* Syntax.print: 抽象構文木をプリントする関数（デバッグ用）  *)

let rec string_of_expr expr = match expr with
    Number (num) -> string_of_int num
  | Real (f) -> string_of_float f
  | Variable (name) -> name
  | Op (arg1, op, arg2) ->
	"(" ^ string_of_expr arg1
	    ^ (match op with
		  Operator.Plus -> "+"
		| Operator.Minus -> "-"
		| Operator.Times -> "*"
		| Operator.Divide -> "/"
		| Operator.Mod -> " mod "
		| Operator.PlusDot -> "+."
		| Operator.MinusDot -> "-."
		| Operator.TimesDot -> "*."
		| Operator.DivideDot -> "/.")
	    ^ string_of_expr arg2 ^ ")"
  | IfEqual (arg1, arg2, arg3, arg4) ->
	"if " ^ string_of_expr arg1
	^ "=" ^ string_of_expr arg2
	^ " then " ^ string_of_expr arg3
	^ " else " ^ string_of_expr arg4
  | IfLess (arg1, arg2, arg3, arg4) ->
	"if " ^ string_of_expr arg1
	^ "<" ^ string_of_expr arg2
	^ " then " ^ string_of_expr arg3
	^ " else " ^ string_of_expr arg4
  | Let ((name, _), arg1, arg2) ->
	"let " ^ name ^ "="
	^ string_of_expr arg1
	^ " in " ^ string_of_expr arg2
  | LetRec ((fun_name, _), arg_list, arg1, arg2) ->
	"let rec " ^ fun_name
	^ List.fold_left (fun str (arg, _) -> str ^ " " ^ arg) "" arg_list
	^ " =\n"
	^ string_of_expr arg1
	^ "\nin\n\n" ^ string_of_expr arg2
  | Application (name, args) ->
	"(" ^ string_of_expr name
	^ List.fold_left (fun str arg -> str ^ " " ^ string_of_expr arg) "" args
	^ ")"

let print expr =
  let str = string_of_expr expr
  in (print_string str;
      print_newline ())
