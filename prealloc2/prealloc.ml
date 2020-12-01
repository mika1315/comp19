(* レジスタ割り当て前処理 *)

open First
open Register

(* メイン *)

let rec f_expr expr env = match expr with
    Number (num) -> Number (num)
  | Real (f) -> Real (f)
  | Variable (name) -> Variable (name)
  | Op (name1, op, name2) -> Op (name1, op, name2)
  | IfEqual (name1, name2, expr3, expr4) -> IfEqual (name1, name2, f_expr expr3 env, f_expr expr4 env)
  | IfLess (name1, name2, expr3, expr4) -> IfLess (name1, name2, f_expr expr3 env, f_expr expr4 env)
  | Let ((name, t), expr1, expr2) -> Let ((name, t), f_expr expr1 env, f_expr expr2 env)
  | Application (name, name_list) ->
    let counter = ref 0 in
    let register_list =
      List.map (fun name_var -> counter := !counter + 1;
                 Register.make_register !counter) name_list in
    let app register name rest =
      Let ((register, Type.gen_type()), Variable (name), rest) in
    List.fold_right2 app register_list name_list (Application (name, register_list))

let f_def env (FunDef ((name, t), params, expr)) =
  let counter = ref 0 in
  let register_list =
    List.map (fun name_var -> counter := !counter + 1;
               (Register.make_register !counter, Type.gen_type())) params in
  let app (name, type1) (register, type2) rest =
    Let ((name, type1), Variable (register), rest) in
  let first_expr = 
    List.fold_right2 app params register_list (f_expr expr env) in
  FunDef ((name, t), register_list, first_expr) 

let f_program (Program (def_list, expr)) env =
  let rec def_pro env def_list = match def_list with
      [] -> []
    | first :: rest -> f_def env first :: def_pro env rest
  in Program ((def_pro env def_list), (f_expr expr env))

(* Prealloc.f : First.prog_t -> First.prog_t *)

let f program = f_program program Env.empty_env
