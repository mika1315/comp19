(* Closure.t: クロージャ変換後の抽象構文木の型 *)

type closure_t = Cls of (string * Type.t) * (string * Type.t) list

type t = Number of int
       | Real of float
       | Variable of string
       | Op of string * Operator.t * string
       | IfEqual of string * string * t * t
       | IfLess of string * string * t * t
       | Let of (string * Type.t) * t * t
       | LetClosure of (string * Type.t) * closure_t * t
       | AppC of string (* 変数名 *) * string list
       | AppD of string (* ラベル名 *) * string list

type def_t = FunDef of (string * Type.t) * (string * Type.t) list *
					   (string * Type.t) list * t

type prog_t = Program of def_t list * t

(* Set モジュール *)
module ValSet = Set.Make (String)

(* Closure.print: 抽象構文木をプリントする関数（デバッグ用）  *)

let type_on = ref false	(* 型推論を実装したら true にする *)

let indent i = String.make i ' '

let string_of_closure (Cls ((f, _), lst)) = match lst with
    [] -> "[" ^ f ^ "]"
  | (x, _) :: rest ->
	"[" ^ f ^ ", [" ^
	List.fold_left (fun str (y, _) -> str ^ "," ^ y)
		       x
		       rest
	^ "]]"

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
  | LetClosure ((name, t), closure, arg) ->
        (if !type_on
	 then "let_closure (" ^ name ^ ":" ^ Type.to_string t ^ ")="
	 else "let_closure " ^ name ^ "=")
	^ string_of_closure closure
	^ " in\n"
	^ indent i ^ string_of_expr arg i
  | AppC (name, args) ->
	"[" ^ name ^ " " ^
	(match args with
	    [] -> ""
	  | arg :: args ->
		arg ^
		List.fold_right (fun a rest -> " " ^ a ^ rest) args "")
	^ "]"
  | AppD (name, args) ->
	"(" ^ name ^ " " ^
	(match args with
	    [] -> ""
	  | arg :: args ->
		arg ^
		List.fold_right (fun a rest -> " " ^ a ^ rest) args "")
	^ ")"

let rec string_of_def (FunDef ((f, _), fvs, params, body)) =
  match fvs with
      [] -> "let rec " ^ f ^ " " ^
	      List.fold_left (fun str (x, t) ->
		if !type_on
		then str ^ "(" ^ x ^ " : " ^ Type.to_string t ^ ") "
		else str ^ x ^ " ")
	      ""
	      params ^ "=\n  " ^
	    string_of_expr body 2 ^ "\n"
    | (y, t) :: rest ->
	    "let rec " ^
	    f ^ List.fold_left (fun str (y, t) ->
		  if !type_on
		  then str ^ ", " ^ y ^ " : " ^ Type.to_string t
		  else str ^ "," ^ y)
		(" [" ^ if !type_on then y ^ " : " ^ Type.to_string t else y)
		rest ^ "] " ^
		List.fold_left (fun str (x, t) ->
		  if !type_on
		  then str ^ "(" ^ x ^ " : " ^ Type.to_string t ^ ") "
		  else str ^ x ^ " ")
		""
		params ^ "=\n  " ^
	      string_of_expr body 2 ^ "\n"

let rec string_of_prog (Program (def_list, expr)) =
  "{\n"
   ^ List.fold_left (fun str def -> str ^ string_of_def def)
		    "" def_list
   ^ "}\n"
   ^ string_of_expr expr 0

let print prog =
  let str = string_of_prog prog
  in (print_string str;
      print_newline ())

(* クロージャ変換後の式 E の中の自由変数を返す関数 *)
(* Closure.free_v : Closure.t -> string set *)
let rec free_v expr = match expr with
    Number (num) -> ValSet.empty
  | Real (f) -> ValSet.empty
  | Variable (name) -> ValSet.singleton name
  | Op (name1, op, name2) ->
    List.fold_right ValSet.add [name1; name2] ValSet.empty
  | IfEqual (name1, name2, expr3, expr4) ->
    let set_name = List.fold_right ValSet.add [name1; name2] ValSet.empty in
    ValSet.union set_name (ValSet.union (free_v expr3) (free_v expr4))
  | IfLess (name1, name2, expr3, expr4) ->
    let set_name = List.fold_right ValSet.add [name1; name2] ValSet.empty in
    ValSet.union set_name (ValSet.union (free_v expr3) (free_v expr4))
  | Let ((name, t), expr1, expr2) ->
    ValSet.union (free_v expr1) (ValSet.remove name (free_v expr2))
  | LetClosure ((name, t), Cls ((f, typ), lst), expr) ->
    let val_lst = List.map (fun (v, t) -> v) lst in
    let val_set = List.fold_right ValSet.add val_lst ValSet.empty in
    ValSet.remove name (ValSet.union val_set (free_v expr))
  | AppC (name, args) ->
    let args_set = List.fold_right ValSet.add args ValSet.empty in
    ValSet.add name args_set
  | AppD (name, args) ->
    List.fold_right ValSet.add args ValSet.empty

(* クロージャ変換のメイン *)
let def_list = ref []

(* 簡易クロージャ変換。s は直接呼び出しても構わない関数の集合 (この中ではずっと s は空) *)
let s = ValSet.empty

(* Closure.def_list : Knormal.t -> string set -> Closure.t *)
let rec c1 expr s def_list = match expr with
    Knormal.Number (num) -> Number (num)
  | Knormal.Real (f) -> Real (f)
  | Knormal.Variable (name) -> Variable (name)
  | Knormal.Op (name1, op, name2) -> Op (name1, op, name2)
  | Knormal.IfEqual (name1, name2, expr3, expr4) ->
    IfEqual (name1, name2, c1 expr3 s def_list, c1 expr4 s def_list)
  | Knormal.IfLess (name1, name2, expr3, expr4) ->
    IfLess (name1, name2, c1 expr3 s def_list, c1 expr4 s def_list)
  | Knormal.Let ((name, t), expr1, expr2) ->
    Let ((name, t), c1 expr1 s def_list, c1 expr2 s def_list)
  | Knormal.LetRec ((name, t), params, expr1, expr2) ->
    let new_expr1 = c1 expr1 s def_list in
    let new_expr2 = c1 expr2 s def_list in
    let param_lst = List.map (fun (v, t) -> v) params in
    let param_set = List.fold_right ValSet.add param_lst ValSet.empty in
    let y_set = ValSet.diff (free_v new_expr1) (ValSet.add name param_set) in
    let y_lst_less_type = ValSet.elements y_set in
    let y_lst = List.map (fun y -> (y, Type.gen_type ())) y_lst_less_type in
    def_list := (FunDef ((name, t), y_lst, params, new_expr1)) :: !def_list;
    LetClosure ((name, t), Cls ((name, t), y_lst), new_expr2)
  | Knormal.Application (name, args) ->
    if ValSet.mem name s then AppD (name, args)
    else AppC (name, args)

(* 多少賢いクロージャ変換 *)
(* Closure.def_list : Knormal.t -> string set -> Closure.t *)
let rec c2 expr s def_list = match expr with
    Knormal.Number (num) -> Number (num)
  | Knormal.Real (f) -> Real (f)
  | Knormal.Variable (name) -> Variable (name)
  | Knormal.Op (name1, op, name2) -> Op (name1, op, name2)
  | Knormal.IfEqual (name1, name2, expr3, expr4) ->
    IfEqual (name1, name2, c2 expr3 s def_list, c2 expr4 s def_list)
  | Knormal.IfLess (name1, name2, expr3, expr4) ->
    IfLess (name1, name2, c2 expr3 s def_list, c2 expr4 s def_list)
  | Knormal.Let ((name, t), expr1, expr2) ->
    Let ((name, t), c2 expr1 s def_list, c2 expr2 s def_list)
  | Knormal.LetRec ((name, t), params, expr1, expr2) ->
    let new_expr1 = c2 expr1 (ValSet.add name s) def_list in
    let param_lst = List.map (fun (v, t) -> v) params in
    let param_set = List.fold_right ValSet.add param_lst ValSet.empty in
    if ValSet.diff (free_v new_expr1) param_set = ValSet.empty
    then
      let new_expr2 = c2 expr2 (ValSet.add name s) def_list in
      def_list := (FunDef ((name, t), [], params, new_expr1)) :: !def_list;
      LetClosure ((name, t), Cls ((name, t), []), new_expr2)
    else
      let new_expr1_1 = c2 expr1 s def_list in
      let new_expr2_2 = c2 expr2 s def_list in
      let y_set = ValSet.diff (free_v new_expr1_1) (ValSet.add name param_set) in
      let y_lst_less_type = ValSet.elements y_set in
      let y_lst = List.map (fun y -> (y, Type.gen_type ())) y_lst_less_type in
      def_list := (FunDef ((name, t), y_lst, params, new_expr1_1)) :: !def_list;
      LetClosure ((name, t), Cls ((name, t), y_lst), new_expr2_2)
  | Knormal.Application (name, args) ->
    if ValSet.mem name s then AppD (name, args)
    else AppC (name, args)

(* もう少し賢いクロージャ変換 *)
(* Closure.def_list : Knormal.t -> string set -> Closure.t *)
let rec c3 expr s def_list = match expr with
    Knormal.Number (num) -> Number (num)
  | Knormal.Real (f) -> Real (f)
  | Knormal.Variable (name) -> Variable (name)
  | Knormal.Op (name1, op, name2) -> Op (name1, op, name2)
  | Knormal.IfEqual (name1, name2, expr3, expr4) ->
    IfEqual (name1, name2, c3 expr3 s def_list, c3 expr4 s def_list)
  | Knormal.IfLess (name1, name2, expr3, expr4) ->
    IfLess (name1, name2, c3 expr3 s def_list, c3 expr4 s def_list)
  | Knormal.Let ((name, t), expr1, expr2) ->
    Let ((name, t), c3 expr1 s def_list, c3 expr2 s def_list)
  | Knormal.LetRec ((name, t), params, expr1, expr2) ->
    let new_expr1 = c3 expr1 (ValSet.add name s) def_list in
    let param_lst = List.map (fun (v, t) -> v) params in
    let param_set = List.fold_right ValSet.add param_lst ValSet.empty in
    if ValSet.is_empty (ValSet.diff (free_v new_expr1) param_set)
    then
      let new_expr2 = c3 expr2 (ValSet.add name s) def_list in
      def_list := (FunDef ((name, t), [], params, new_expr1)) :: !def_list;
      if ValSet.mem name (free_v new_expr2)
      then LetClosure ((name, t), Cls ((name, t), []), new_expr2)
      else new_expr2
    else
      let new_expr1_1 = c3 expr1 s def_list in
      let new_expr2_2 = c3 expr2 s def_list in
      let y_set = ValSet.diff (free_v new_expr1_1) (ValSet.add name param_set) in
      let y_lst_less_type = ValSet.elements y_set in
      let y_lst = List.map (fun y -> (y, Type.gen_type ())) y_lst_less_type in
      def_list := (FunDef ((name, t), y_lst, params, new_expr1_1)) :: !def_list;
      LetClosure ((name, t), Cls ((name, t), y_lst), new_expr2_2)
  | Knormal.Application (name, args) ->
    if ValSet.mem name s then AppD (name, args)
    else AppC (name, args)
        
let rec c_program expr def_list =
  let new_expr = c3 expr s def_list in
  Program (!def_list, new_expr)
  
(* Closure.f: クロージャ変換の入口 *)
(* Closure.f : Knormal.t -> Closure.prog_t *)
let f program = c_program program def_list
