(* 単純なレジスタ割り当て *)

open Closure
open Register
    
(* メイン *)

let rec g_expr expr env r_counter f_counter =
  match expr with
    Number (num) -> Number (num)
  | Real (f) -> Real (f)
  | Variable (name) -> Variable (Env.get env name)
  | Op (name1, op, name2) -> Op (Env.get env name1, op, Env.get env name2)
  | IfEqual (name1, name2, expr3, expr4) ->
    let g_expr3 = g_expr expr3 env r_counter f_counter in
    let g_expr4 = g_expr expr4 env r_counter f_counter in
    IfEqual (Env.get env name1, Env.get env name2, g_expr3, g_expr4)
  | IfLess (name1, name2, expr3, expr4) ->
    let g_expr3 = g_expr expr3 env r_counter f_counter in
    let g_expr4 = g_expr expr4 env r_counter f_counter in
    IfLess (Env.get env name1, Env.get env name2, g_expr3, g_expr4)
  | Let ((name, t), expr1, expr2) ->
    (* 変数がすでにレジスタだった場合 *)
    if Register.is_register name
    then Let ((name, t), g_expr expr1 env r_counter f_counter, g_expr expr2 env r_counter f_counter)
    (* 変数にまだレジスタが割り当てられていない場合 *)
    else if t != Type.Float
    then
      let register = r_counter := !r_counter - 1;
        Register.make_register !r_counter in
      let g_expr1 = g_expr expr1 env r_counter f_counter in
      let g_expr2 = g_expr expr2 (Env.add env name register) r_counter f_counter in
      Let ((register, t), g_expr1, g_expr2)
    else
      let register = f_counter := !f_counter - 1;
        Register.make_fregister !f_counter in
      let g_expr1 = g_expr expr1 env r_counter f_counter in
      let g_expr2 = g_expr expr2 (Env.add env name register) r_counter f_counter in
      Let ((register, t), g_expr1, g_expr2)
  | LetClosure ((name, t), Cls ((f, typ), lst), expr) ->
    if t != Type.Float
    then
      let register = r_counter := !r_counter - 1;
        Register.make_register !r_counter in
      let new_env = Env.add env name register in
      let new_expr = g_expr expr new_env r_counter f_counter in
      let new_lst = List.map (fun (y, t) -> ((Env.get new_env y), t)) lst in
      LetClosure ((register, t), Cls ((f, typ), new_lst), new_expr)
    else
      let register = f_counter := !f_counter - 1;
        Register.make_fregister !f_counter in
      let new_env = Env.add env name register in
      let new_expr = g_expr expr new_env r_counter f_counter in
      let new_lst = List.map (fun (y, t) -> ((Env.get new_env y), t)) lst in
      LetClosure ((register, t), Cls ((f, typ), new_lst), new_expr)
  | AppC (name, name_list) -> AppC (name, name_list)
  | AppD (name, name_list) -> AppD (name, name_list) 

let g_def (FunDef ((name, t), y_lst, params, expr)) =
  let f_counter = ref 0 in
  let rec counter params = match params with
      [] -> f_counter := !f_counter
    | (first_register, r_type) :: rest ->
      if is_fregister first_register
      then
        (f_counter := !f_counter + 1;
         counter rest)
      else
        (f_counter := !f_counter;
         counter rest) in
  counter params;
  let param_length = List.length params in
  let r_counter = ref (param_length - !f_counter) in
  let make_register (y, typ) =
    let register =
      if t != Type.Float
      then
        (r_counter := !r_counter + 1;
         Register.make_register !r_counter)
      else
        (f_counter := !f_counter + 1;
         Register.make_fregister !f_counter)
    in (register, typ) in
  let new_y_lst = List.map make_register y_lst in
  let pre_env = List.map (fun (param, t) -> (param, param)) params in
  let env = Env.add pre_env (Register.make_register 0) (Register.make_register 0) in
  let new_env = List.map2 (fun (register, typ) (y, t) -> (y, register)) new_y_lst y_lst in
  FunDef ((name, t), new_y_lst, params, (g_expr expr (env @ new_env) (ref 12) (ref 15)))

let g_program (Program (def_list, expr)) =
  Program (List.map g_def def_list, (g_expr expr Env.empty_env (ref 12) (ref 15)))

(* Alloc.f : Closure.prog_t -> Closure.prog_t *)

(* 単純なレジスタ割り当て *)
let f program = g_program program
