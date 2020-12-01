(* β変換 *)

open Knormal

(* Set モジュール *)
module ValSet = Set.Make (String)

(* 式 expr の中の自由変数を返す関数 *)
(* Beta.free_v : Knormal.t -> Set.t string *)
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
  | Let ((name, t), Variable (val_name), expr2) ->
    ValSet.add [val_name] ValSet.empty
  | Let ((name, t), expr1, expr2) ->
    ValSet.union (free_v expr1) (free_v expr2)
  | LetRec ((name, t), params, expr1, expr2) ->
    let rec set_params param_list = match param_list with
        [] -> ValSet.empty
      | (variable, type_t) :: rest -> ValSet.add variable (set_params rest)
    in
    ValSet.union (set_params params) (ValSet.union (free_v expr1) (free_v expr2))    
  | Application (name, name_list) ->
    List.fold_right ValSet.add name_list ValSet.empty

(* メイン *)

let rec g expr env = match expr with
    Number (num) -> expr
  | Real (f) -> expr
  | Variable (name) -> Variable (Env.get env name)
  | Op (name1, op, name2) ->
    Op (Env.get env name1,
        op,
        Env.get env name2)
  | IfEqual (name1, name2, expr3, expr4) ->
    IfEqual (Env.get env name1,
             Env.get env name2,
             g expr3 env,
	     g expr4 env)
  | IfLess (name1, name2, expr3, expr4) ->
    IfLess (Env.get env name1,
            Env.get env name2,
	    g expr3 env,
	    g expr4 env)
  | Let ((name, t), Variable (val_name), expr2) ->
    g expr2 (Env.add env name (Env.get env val_name))
  | Let ((name, t), expr1, expr2) -> 
    let new_expr1 = g expr1 env in
    let new_expr2 = g expr2 env in
    Let ((name, t), new_expr1, new_expr2)
  | LetRec ((name, t), params, expr1, expr2) ->
    let new_expr1 = g expr1 env in
    let new_expr2 = g expr2 env in
    LetRec ((name, t), params, new_expr1, new_expr2)
  | Application (name, name_list) ->
    Application (name, List.map (fun var -> Env.get env var) name_list)

(* Beta.f : Knormal.t -> Knormal.t *)

let f expr =
  let variables = ValSet.elements (free_v expr) in
  let rec all_add val_list = match val_list with
      [] -> Env.empty_env
    | first :: rest -> Env.add (all_add rest) first first
  in
  g expr (all_add variables)
