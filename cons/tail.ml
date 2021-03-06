(* Intel 用コード生成 *)

open Closure
open Register

(* registers *)

let r_sp = "_R_sp"
let r_bp = "_R_hp"
let r_hp = "_R_hp"
let r_ax = "_R_ax"
let r_dx = "_R_dx"
let f_tmp = "_F_tmp"

(* instructions *)

let label l = l ^ ":\n"
let movqi i r2    = "	movq $" ^ string_of_int i ^ ", " ^ r2 ^ "\n"
let movqf i r2  = "	movq $" ^ string_of_float i ^ ", " ^ r2 ^ "\n"
let movq r1 r2    = "	movq " ^ r1 ^ ", " ^ r2 ^ "\n"
let movq2 i r1 r2 = "	movq " ^ string_of_int i ^ "(" ^ r1 ^ "), " ^ r2 ^"\n"
let movq3 r1 i r2 = "	movq " ^ r1 ^ ", " ^ string_of_int i ^ "(" ^ r2 ^ ")\n"
let leaq f r    = "	leaq " ^ f ^ "(%rip), " ^ r ^ "\n"
let addq r1 r2  = "	addq " ^ r1 ^ ", " ^ r2 ^ "\n"
let addqi i r2  = "	addq $" ^ string_of_int i ^ ", " ^ r2 ^ "\n"
let subq r1 r2  = "	subq " ^ r1 ^ ", " ^ r2 ^ "\n"
let subqi i r2  = "	subq $" ^ string_of_int i ^ ", " ^ r2 ^ "\n"
let imulq r1 r2 = "	imulq " ^ r1 ^ ", " ^ r2 ^ "\n"
let cqto =	"	cqto\n"
let idivq r =	"	idivq " ^ r ^ "\n"
let cmpq r1 r2 = "	cmpq " ^ r1 ^ ", " ^ r2 ^ "\n"
let jne l =	"	jne " ^ l ^ "\n"
let jle l =	"	jle " ^ l ^ "\n"
let jmp l =	"	jmp " ^ l ^ "\n"
let jmps r =	"	jmp *" ^ r ^ "\n"
let pushq r =	"	pushq " ^ r ^ "\n"
let popq r  =	"	popq " ^ r ^ "\n"
let call f =	"	call " ^ f ^ "\n"
let calls r =	"	call *" ^ r ^ "\n"
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
			movq "%rdi" r_bp ^
		"    # main program start\n"
let last =	"    # main program end\n" ^
			movq (make_register 0) r_ax ^
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
  
(* load/store free variables *)

let rec load_fv fv_list i r0 = match fv_list with
    [] -> ""
  | (r, t) :: rest ->
	movq2 i r0 r ^
	load_fv rest (i+8) r0

let rec store_fv fv_list i r0 = match fv_list with
    [] -> ""
  | (r, t) :: rest ->
        movq3 r i r0 ^
	store_fv rest (i+8) r0

(* メイン *)
(* クロージャ変換後の式 E の中の自由変数を返す関数 *)
(* Tail.free_v : Closure.t -> string set *)
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

(* counter を定義 *)
let counter = ref 22
    
(* i を受け取り l_i を作る *)
(* Tail.make_label : int -> string *)
let make_label counter =
    counter := !counter - 1;
    "l_" ^ (string_of_int !counter)

(* レジスタ割り当てのすんだ一階の言語に対するコード生成 (末尾呼び出し最適化) *)
(* Tail.t_expr : First.t -> string -> string list -> string *)
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
  | LetClosure ((name, t), Cls ((f, typ), lst), expr) ->
    movq r_hp name ^
    addqi (8 * ((List.length lst) + 1)) r_hp ^
    leaq f r_dx ^
    movq3 r_dx 0 name ^
    let number = ref 0 in
    (let rec new_movq3 lst = match lst with
          [] -> ""
        | (register, typ) :: rest ->
          number := !number + 1;
          movq3 register (8 * !number) name ^ new_movq3 rest
     in new_movq3 lst) ^
    g_expr expr z live counter
  | AppC (name, register_list) ->
    push_live (ValSet.elements live) ^
    movq2 0 (make_register 0) r_dx ^
    calls r_dx ^
    (if z = (make_register 0) then ""
    else movq (make_register 0) z)
    ^ pop_live (ValSet.elements live)
  | AppD (name, register_list) ->
    push_live (ValSet.elements live) ^ call name ^
    (if z = (make_register 0) then ""
    else movq (make_register 0) z)
         ^ pop_live (ValSet.elements live)

let rec t_expr expr z live counter = match expr with
    Number (num) -> movqi num (make_register 0) ^ ret
  | Real (f) -> movqf f (make_fregister 0) ^ ret
  | Variable (name) ->
    if name = (make_register 0) then ret
    else movq name (make_register 0) ^ ret
  | Op (name1, op, name2) ->
    (match op with
       Operator.Plus ->
       movq name1 r_ax ^ addq name2 r_ax ^ movq r_ax (make_register 0) ^ ret
     | Operator.Minus ->
       movq name1 r_ax ^ subq name2 r_ax ^ movq r_ax (make_register 0) ^ ret
     | Operator.Times ->
       movq name1 r_ax ^ imulq name2 r_ax ^ movq r_ax (make_register 0) ^ ret
     | Operator.Divide ->
       movq name1 r_ax ^ cqto ^
       idivq name2 ^ movq r_ax (make_register 0) ^ ret
     | Operator.Mod ->
       movq name1 r_ax ^ cqto ^
       idivq name2 ^ movq r_dx (make_register 0) ^ ret
     | Operator.PlusDot ->
       movq name1 r_ax ^ addq name2 r_ax ^ movq r_ax (make_fregister 0) ^ ret
     | Operator.MinusDot ->
       movq name1 r_ax ^ subq name2 r_ax ^ movq r_ax (make_fregister 0) ^ ret
     | Operator.TimesDot ->
       movq name1 r_ax ^ imulq name2 r_ax ^ movq r_ax (make_fregister 0) ^ ret
     | Operator.DivideDot ->
       movq name1 r_ax ^ cqto ^
       idivq name2 ^ movq r_ax (make_fregister 0) ^ ret
    )
  | IfEqual (name1, name2, expr3, expr4) ->
    let label1 = make_label counter in
    cmpq name1 name2 ^ (jne label1) ^
    t_expr expr3 z live counter ^ label label1 ^ t_expr expr4 z live counter
  | IfLess (name1, name2, expr3, expr4) ->
    let label1 = make_label counter in
    cmpq name1 name2 ^ (jle label1) ^
    t_expr expr3 z live counter ^ label label1 ^ t_expr expr4 z live counter
  | Let ((name, t), expr1, expr2) ->
    g_expr expr1 name (ValSet.remove name (free_v expr2)) counter ^
    t_expr expr2 z live counter
  | LetClosure ((name, t), Cls ((f, typ), lst), expr) ->
    movq r_hp name ^
    addqi (8 * ((List.length lst) + 1)) r_hp ^
    leaq f r_dx ^
    movq3 r_dx 0 name ^
    let number = ref 0 in
    (let rec new_movq3 lst = match lst with
          [] -> ""
        | (register, typ) :: rest ->
          number := !number + 1;
          movq3 register (8 * !number) name ^ new_movq3 rest
     in new_movq3 lst) ^
    t_expr expr z live counter
  | AppC (name, register_list) ->
    push_live (ValSet.elements live) ^
    movq2 0 (make_register 0) r_dx ^
    calls r_dx ^
    (if z = (make_register 0) then ""
    else movq (make_register 0) z)
    ^ pop_live (ValSet.elements live)
  | AppD (name, register_list) -> jmp name

(* Code.g_def : First.def_t -> string *)
let g_def (FunDef ((name, t), register_lst, params, expr)) =
  label name ^
  let number = ref 0 in
    (let rec new_movq2 lst = match lst with
          [] -> ""
        | (register, typ) :: rest ->
          number := !number + 1;
          movq2 (8 * !number) (make_register 0) register ^ new_movq2 rest
     in new_movq2 register_lst)
      ^ (t_expr expr (make_register 0) ValSet.empty counter) ^ ret

(* Code.g_program : First.prog_t -> string *)
let g_program (Program (def_list, expr)) =
  top ^
  (let rec def_all list =
    match list with
      [] -> ""
    | first :: rest -> (g_def first) ^ (def_all rest) in def_all def_list)
  ^ middle ^ (g_expr expr (make_register 0) ValSet.empty counter) ^
  last
    

(* Code.f : Closure.prog_t -> string *)

let f program = g_program program
