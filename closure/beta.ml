(* β変換 *)

open Knormal

(* 例外処理(keyが見つからなかったらそのまま返す) *)
let env_get env key =
  try
    Env.get env key
  with
  Env.UnboundVariable key -> key

(* メイン *)

let rec g expr env = match expr with
    Number (num) -> expr
  | Real (f) -> expr
  | Variable (name) -> Variable (env_get env name)
  | Op (name1, op, name2) ->
    Op (env_get env name1,
        op,
        env_get env name2)
  | IfEqual (name1, name2, expr3, expr4) ->
    IfEqual (env_get env name1,
             env_get env name2,
             g expr3 env,
	     g expr4 env)
  | IfLess (name1, name2, expr3, expr4) ->
    IfLess (env_get env name1,
            env_get env name2,
	    g expr3 env,
	    g expr4 env)
  | Let ((name, t), Variable (val_name), expr2) ->
    g expr2 (Env.add env name (env_get env val_name))
  | Let ((name, t), expr1, expr2) -> 
    let new_expr1 = g expr1 env in
    let new_expr2 = g expr2 env in
    Let ((name, t), new_expr1, new_expr2)
  | LetRec ((name, t), params, expr1, expr2) ->
    let new_expr1 = g expr1 env in
    let new_expr2 = g expr2 env in
    LetRec ((name, t), params, new_expr1, new_expr2)
  | Application (name, name_list) ->
    Application (name, List.map (fun var -> env_get env var) name_list)

(* Beta.f : Knormal.t -> Knormal.t *)

let f expr = g expr Env.empty_env
