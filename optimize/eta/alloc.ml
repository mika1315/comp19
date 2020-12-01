(* 単純なレジスタ割り当て *)

open First
open Register

(* メイン *)

let rec g_expr expr env counter =
  match expr with
    Number (num) -> Number (num)
  | Real (f) -> Real (f)
  | Variable (name) -> Variable (Env.get env name)
  | Op (name1, op, name2) -> Op (Env.get env name1, op, Env.get env name2)
  | IfEqual (name1, name2, expr3, expr4) ->
    let g_expr3 = g_expr expr3 env counter in
    let g_expr4 = g_expr expr4 env counter in
    IfEqual (Env.get env name1, Env.get env name2, g_expr3, g_expr4)
  | IfLess (name1, name2, expr3, expr4) ->
    let g_expr3 = g_expr expr3 env counter in
    let g_expr4 = g_expr expr4 env counter in
    IfLess (Env.get env name1, Env.get env name2, g_expr3, g_expr4)
  | Let ((name, t), expr1, expr2) ->
    if Register.is_register name
    then Let ((name, t), g_expr expr1 env counter, g_expr expr2 env counter)
    else
      let register =
        counter := !counter - 1;
        Register.make_register !counter in
      let g_expr1 = g_expr expr1 env counter in
      let g_expr2 = g_expr expr2 (Env.add env name register) counter in
      Let ((register, t), g_expr1, g_expr2)
  | Application (name, register_list) -> Application (name, register_list)

let g_def (FunDef ((name, t), params, expr)) =
  let env = List.map (fun (param, t) -> (param, param)) params in 
  FunDef ((name, t), params, (g_expr expr env (ref 12)))

let g_program (Program (def_list, expr)) =
  Program (List.map g_def def_list, (g_expr expr Env.empty_env (ref 12)))

(* Alloc.f : First.prog_t -> First.prog_t *)

(* 単純なレジスタ割り当て *)
let f program = g_program program
