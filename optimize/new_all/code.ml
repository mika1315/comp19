(* Intel 用コード生成 *)

open First
open Register

(* Set モジュール *)
module ValSet = Set.Make (String)

(* registers *)

let r_sp = "_R_sp"
let r_bp = "_R_bp"
let r_ax = "_R_ax"
let r_dx = "_R_dx"
let f_tmp = "_F_tmp"

(* instructions *)

let label l = l ^ ":\n"
let movqi i r2  = "	movq $" ^ string_of_int i ^ ", " ^ r2 ^ "\n"
let movq r1 r2  = "	movq " ^ r1 ^ ", " ^ r2 ^ "\n"
let addq r1 r2  = "	addq " ^ r1 ^ ", " ^ r2 ^ "\n"
let subq r1 r2  = "	subq " ^ r1 ^ ", " ^ r2 ^ "\n"
let subqi i r2  = "	subq " ^ string_of_int i ^ ", " ^ r2 ^ "\n"
let imulq r1 r2 = "	imulq " ^ r1 ^ ", " ^ r2 ^ "\n"
let cqto =	"	cqto\n"
let idivq r =	"	idivq " ^ r ^ "\n"
let cmpq r1 r2 = "	cmpq " ^ r1 ^ ", " ^ r2 ^ "\n"
let jne l =	"	jne " ^ l ^ "\n"
let jle l =	"	jle " ^ l ^ "\n"
let jmp l =	"	jmp " ^ l ^ "\n"
let pushq r =	"	pushq " ^ r ^ "\n"
let popq r  =	"	popq " ^ r ^ "\n"
let call f =	"	call " ^ f ^ "\n"
let ret =	"	ret\n"

(* headers *)

let top =	"	.text\n"
let middle =	"\n" ^
		"	.globl _asm_main\n" ^
		"_asm_main: # main entry point\n" ^
			pushq "%rbx" ^
			pushq "%r12" ^
			pushq "%r13" ^
			pushq "%r14" ^
			pushq "%r15" ^
			pushq r_bp ^
			movq r_sp r_bp ^
		"    # main program start\n"
let last =	"    # main program end\n" ^
			movq (make_register 0) r_ax ^
			movq r_bp r_sp ^
			popq r_bp ^
			popq "%r15" ^
			popq "%r14" ^
			popq "%r13" ^
			popq "%r12" ^
			popq "%rbx" ^
			ret

(* push/pop registers *)

let rec push_live live = match live with
    [] -> ""
  | var :: rest -> pushq var ^ push_live rest

let rec pop_live live = match live with
    [] -> ""
  | var :: rest -> pop_live rest ^ popq var

(* メイン *)

(* 式 expr の中の自由変数を返す関数 *)
(* Code.free_v : First.t -> string list *)
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
  | Application (name, register_list) ->
    List.fold_right ValSet.add register_list ValSet.empty

(* counter を定義 *)
let counter = ref 22
    
(* i を受け取り l_i を作る *)
(* Code.make_label : int -> string *)
let make_label counter =
    counter := !counter - 1;
    "l_" ^ (string_of_int !counter)

(* レジスタ割り当てのすんだ一階の言語に対するコード生成 *)
(* Code.g_expr : First.t -> string -> string list -> string *)
exception NotSupported
  
let rec g_expr expr z live counter = match expr with
    Number (num) -> movqi num z
  | Real (f) -> raise NotSupported
  | Variable (name) ->
    if name = z then ""
    else movq name z
  | Op (name1, op, name2) ->
    (match op with
       Operator.Plus ->
       movq name1 r_ax ^ addq name2 r_ax ^ movq r_ax z
     | Operator.Minus ->
       movq name1 r_ax ^ subq name2 r_ax ^ movq r_ax z
     | Operator.Times ->
       movq name1 r_ax ^ imulq name2 r_ax ^ movq r_ax z
     | Operator.Divide ->
       movq name1 r_ax ^ cqto ^
       idivq name2 ^ movq r_ax z
     | Operator.Mod ->
       movq name1 r_ax ^ cqto ^
       idivq name2 ^ movq r_dx z
     | Operator.PlusDot -> raise NotSupported
     | Operator.MinusDot -> raise NotSupported
     | Operator.TimesDot -> raise NotSupported
     | Operator.DivideDot -> raise NotSupported)
  | IfEqual (name1, name2, expr3, expr4) ->
    let label1 = make_label counter in
    let label2 = make_label counter in
    cmpq name1 name2 ^ (jne label1) ^
    g_expr expr3 z live counter ^ jmp label2 ^
    label label1 ^ g_expr expr4 z live counter ^ label label2
  | IfLess (name1, name2, expr3, expr4) ->
    let label1 = make_label counter in
    let label2 = make_label counter in
    cmpq name1 name2 ^ (jle label1) ^
    g_expr expr3 z live counter ^ jmp label2 ^
    label label1 ^ g_expr expr4 z live counter ^ label label2
  | Let ((name, t), expr1, expr2) ->
    g_expr expr1 name (ValSet.union live (ValSet.remove name (free_v expr2))) counter ^
    g_expr expr2 z live counter 
  | Application (name, register_list) ->
    push_live (ValSet.elements live) ^ call name ^
    (if z = (make_register 0) then ""
    else movq (make_register 0) z)
         ^ pop_live (ValSet.elements live)

(* Code.g_def : First.def_t -> string *)
let g_def (FunDef ((name, t), params, expr)) =
  label name ^ pushq r_bp ^ movq r_sp r_bp
  ^ (g_expr expr (make_register 0) ValSet.empty counter) ^ movq r_bp r_sp
    ^ popq r_bp ^ ret

(* Code.g_program : First.prog_t -> string *)
let g_program (Program (def_list, expr)) =
  top ^
  (let rec def_all list =
    match list with
      [] -> ""
    | first :: rest -> (g_def first) ^ (def_all rest) in def_all def_list)
  ^ middle ^ g_expr expr (make_register 0) ValSet.empty counter ^
  last
    
(* Code.f : First.prog_t -> string *)

let f program = g_program program 
