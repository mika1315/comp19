(* レジスタ割り当て前処理 *)

open Closure
open Register

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

(* メイン *)

let rec f_expr expr env = match expr with
    Number (num) -> Number (num)
  | Real (f) -> Real (f)
  | Variable (name) -> Variable (name)
  | Op (name1, op, name2) -> Op (name1, op, name2)
  | IfEqual (name1, name2, expr3, expr4) -> IfEqual (name1, name2, f_expr expr3 env, f_expr expr4 env)
  | IfLess (name1, name2, expr3, expr4) -> IfLess (name1, name2, f_expr expr3 env, f_expr expr4 env)
  | Let ((name, t), expr1, expr2) -> Let ((name, t), f_expr expr1 env, f_expr expr2 (Env.add env name t))
  | LetClosure ((name, t), Cls ((f, typ), lst), expr) ->
    LetClosure ((name, t), Cls ((f, typ), lst), f_expr expr (Env.add env name t))
  | AppC (name, name_list) ->
    let r_counter = ref 0 in
    let f_counter = ref (-1) in
    let register_list =
      List.map (fun name_var -> if (Env.get env name_var) != Type.Float
                 then (r_counter := !r_counter + 1;
                       Register.make_register !r_counter)
                 else (f_counter := !f_counter + 1;
                       Register.make_fregister !f_counter)) name_list in
    let app register name rest =
      Let ((register, (Env.get env name)), Variable (name), rest) in
    List.fold_right2 app (Register.make_register 0 :: register_list) (name :: name_list) (AppC (Register.make_register 0, register_list))
  | AppD (name, name_list) ->
    let r_counter = ref 0 in
    let f_counter = ref (-1) in
    let register_list =
      List.map (fun name_var -> if (Env.get env name_var) != Type.Float
                 then (r_counter := !r_counter + 1;
                       Register.make_register !r_counter)
                 else (f_counter := !f_counter + 1;
                       Register.make_fregister !f_counter)) name_list in
    let app register name rest =
      if is_fregister register then
        Let ((register, (Env.get env name)), Variable (name), rest)
      else Let ((register, (Env.get env name)), Variable (name), rest) in
    List.fold_right2 app register_list name_list (AppD (name, register_list))

let f_def env (FunDef ((name, t), y_lst, params, expr)) =
  let r_counter = ref 0 in
  let f_counter = ref (-1) in
  let register_list =
    List.map (fun (name_p, t_p) -> if t_p != Type.Float
               then (r_counter := !r_counter + 1;
                     (Register.make_register !r_counter, t_p))
               else (f_counter := !f_counter + 1;
                     (Register.make_fregister !f_counter, t_p))) params in
  let app (name, type1) (register, type2) rest =
    if type1 != Type.Float
    then Let ((name, type1), Variable (register), rest)
    else Let ((name, type1), Variable (register), rest) in
  let new_env = Env.add env name t in
  let first_expr = 
    List.fold_right2 app params register_list (f_expr expr (y_lst @ params @ new_env)) in
  if ValSet.mem name (free_v expr) then
    let new_first_expr =
      Let ((name, t), Variable (Register.make_register 0), first_expr) in
    FunDef ((name, t), y_lst, register_list, new_first_expr)
  else FunDef ((name, t), y_lst, register_list, first_expr)

let f_program (Program (def_list, expr)) env =
  let rec def_pro env def_list = match def_list with
      [] -> []
    | first :: rest -> f_def env first :: def_pro env rest
  in Program ((def_pro env def_list), (f_expr expr env))

(* Prealloc.f : First.prog_t -> First.prog_t *)

let f program = f_program program Env.empty_env
