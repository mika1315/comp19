(* First.t: １階の言語の抽象構文木の型 *)

type t = Number of int
       | Real of float
       | Variable of string
       | Op of string * Operator.t * string
       | IfEqual of string * string * t * t
       | IfLess of string * string * t * t
       | Let of (string * Type.t) * t * t
       | Application of string * string list

type def_t = FunDef of (string * Type.t) * (string * Type.t) list * t

type prog_t = Program of def_t list * t

(* First.print: 抽象構文木をプリントする関数（デバッグ用）  *)

let type_on = ref false (* 型推論を実装したら true にする *)

let indent i = String.make i ' '

let rec string_of_expr expr i = match expr with
    Number (num) -> string_of_int num
  | Real (f) -> string_of_float f
  | Variable (name) -> name
  | Op (arg1, op, arg2) ->
	"(" ^ arg1
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
	    ^ arg2 ^ ")"
  | IfEqual (arg1, arg2, arg3, arg4) ->
	"if " ^ arg1 ^ "=" ^ arg2 ^ "\n"
	^ indent i ^ "then " ^ string_of_expr arg3 (i+5) ^ "\n"
	^ indent i ^ "else " ^ string_of_expr arg4 (i+5)
  | IfLess (arg1, arg2, arg3, arg4) ->
	"if " ^ arg1 ^ "<" ^ arg2 ^ "\n"
	^ indent i ^ "then " ^ string_of_expr arg3 (i+5) ^ "\n"
	^ indent i ^ "else " ^ string_of_expr arg4 (i+5)
  | Let ((name, t), arg1, arg2) ->
	(if !type_on
	 then "let (" ^ name ^ ":" ^ Type.to_string t ^ ")="
	 else "let " ^ name ^ "=")
	^ string_of_expr arg1 (i+5+String.length name)
	^ " in\n"
	^ indent i ^ string_of_expr arg2 i
  | Application (name, args) ->
	"(" ^ name ^ " " ^
	(match args with
	    [] -> ""
	  | arg :: args ->
		arg ^
		List.fold_right (fun a rest -> " " ^ a ^ rest) args "")
	^ ")"

let rec string_of_prog (Program (def_list, expr)) =
  List.fold_right
    (fun (FunDef ((name, _), args, arg1)) str ->
	"let rec " ^ name
	^ List.fold_left
	    (fun str (arg, t) ->
	      if !type_on
	      then str ^ " (" ^ arg ^ ":" ^ Type.to_string t ^ ")"
	      else str ^ " " ^ arg)
	    "" args
	^ " =\n  "
	^ string_of_expr arg1 2 ^ "\n"
	^ "in\n\n" ^ str)
    def_list
    (string_of_expr expr 0)

let print prog =
  let str = string_of_prog prog
  in (print_string str;
      print_newline ())

(* １階の言語への変換プログラムのメイン *)

exception NotSupported

let rec g expr = match expr with
    Knormal.Number (num) -> Number (num)
  | Knormal.Real (f) -> Real (f)
  | Knormal.Variable (name) -> Variable (name)
  | Knormal.Op (arg1, op, arg2) -> Op (arg1, op, arg2)
  | Knormal.IfEqual (arg1, arg2, arg3, arg4) ->
	IfEqual (arg1, arg2, g arg3, g arg4)
  | Knormal.IfLess (arg1, arg2, arg3, arg4) ->
	IfLess (arg1, arg2, g arg3, g arg4)
  | Knormal.Let ((name, typ), arg1, arg2) ->
	Let ((name, typ), g arg1, g arg2)
  | Knormal.LetRec ((name, typ), args, arg1, arg2) ->
	raise NotSupported
  | Knormal.Application (name, args) -> Application (name, args)

let rec g_program program = match program with
    Knormal.LetRec ((name, typ), args, arg1, arg2) ->
	let Program (lst, expr) = g_program arg2 in
	Program (FunDef ((name, typ), args, g arg1) :: lst, expr)
  | _ -> Program ([], g program)

(* First.f: １階の言語への変換プログラムの入口 *)
(* First.f : Knormal.t -> First.prog_t *)
let f expr = g_program expr
