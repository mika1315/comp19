(* β変換 *)

open Knormal

(* key が見つからなかったときに raise される例外 *)
exception UnboundVariable of string
    
(* メイン *)

let rec g expr env = match expr with
    Number (num) -> expr
  | Real (f) -> expr
  | Variable (name) -> Variable (Env.get (Env.add env name name) name)
  | Op (name1, op, name2) ->
    Op (Env.get (Env.add env name1 name1) name1,
        op,
        Env.get (Env.add env name2 name2) name2)
  | IfEqual (name1, name2, expr3, expr4) ->
    IfEqual (Env.get (Env.add env name1 name1) name1,
             Env.get (Env.add env name2 name2) name2,
             g expr3 env,
	     g expr4 env)
  | IfLess (name1, name2, expr3, expr4) ->
    IfLess (Env.get (Env.add env name1 name1) name1,
            Env.get (Env.add env name2 name2) name2,
	    g expr3 env,
	    g expr4 env)
  | Let ((name, t), Variable (val_name), expr2) ->
    g expr2 (Env.add env name (Env.get (Env.add env val_name val_name) val_name))
  | Let ((name, t), expr1, expr2) -> 
    let new_expr1 = g expr1 env in
    let new_expr2 = g expr2 env in
    Let ((name, t), new_expr1, new_expr2)
  | LetRec ((name, t), params, expr1, expr2) ->
    let new_expr1 = g expr1 env in
    let new_expr2 = g expr2 env in
    LetRec ((name, t), params, new_expr1, new_expr2)
  | Application (name, name_list) ->
    Application (Env.get (Env.add env name name) name,
		 List.map (fun var -> Env.get (Env.add env var var) var) name_list)

(* Beta.f : Knormal.t -> Knormal.t *)

let f expr = g expr Env.empty_env
