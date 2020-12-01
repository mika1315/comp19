(* α変換 *)

open Knormal

(* params に新しい名前をつけ、それを環境に追加する *)

let rec add_params env params = match params with
    [] -> env
  | (first, t) :: rest -> add_params (Env.add env first (Gensym.f first)) rest

(* メイン *)

let rec g expr env = match expr with
    Number (num) -> expr
  | Real (f) -> expr
  | Variable (name) -> Variable (Env.get env name)
  | Op (name1, op, name2) -> Op (Env.get env name1, op, Env.get env name2)
  | IfEqual (name1, name2, expr3, expr4) ->
	IfEqual (Env.get env name1, Env.get env name2,
		 g expr3 env,
		 g expr4 env)
  | IfLess (name1, name2, expr3, expr4) ->
	IfLess (Env.get env name1, Env.get env name2,
		g expr3 env,
		g expr4 env)
  | Let ((name, t), expr1, expr2) ->
	let new_name = Gensym.f name in
	let new_expr1 = g expr1 env in
	let new_env = Env.add env name new_name in
	let new_expr2 = g expr2 new_env in
	Let ((new_name, t), new_expr1, new_expr2)
  | LetRec ((name, t), params, expr1, expr2) ->
	let new_name = Gensym.f name in
	let new_env = add_params env params in
	let new_params = List.map (fun (param, t) -> (Env.get new_env param, t))
				  params in
	let new_expr1 = g expr1 (Env.add new_env name new_name) in
	let new_expr2 = g expr2 (Env.add env name new_name) in
	LetRec ((new_name, t), new_params, new_expr1, new_expr2)
  | Application (name, name_list) ->
	Application (Env.get env name,
	      List.map (fun var -> Env.get env var) name_list)

let rec g2 expr env = match expr with
    Number (num) -> expr
  | Real (f) -> expr
  | Variable (name) -> Variable (Env.get2 env name)
  | Op (name1, op, name2) -> Op (Env.get2 env name1, op, Env.get2 env name2)
  | IfEqual (name1, name2, expr3, expr4) ->
	IfEqual (Env.get2 env name1, Env.get2 env name2,
		 g2 expr3 env,
		 g2 expr4 env)
  | IfLess (name1, name2, expr3, expr4) ->
	IfLess (Env.get2 env name1, Env.get2 env name2,
		g2 expr3 env,
		g2 expr4 env)
  | Let ((name, t), expr1, expr2) ->
	let new_name = Gensym.f name in
	let new_expr1 = g2 expr1 env in
	let new_env = Env.add env name new_name in
	let new_expr2 = g2 expr2 new_env in
	Let ((new_name, t), new_expr1, new_expr2)
  | LetRec ((name, t), params, expr1, expr2) ->
	let new_name = Gensym.f name in
	let new_env = add_params env params in
	let new_params = List.map (fun (param, t) -> (Env.get2 new_env param, t))
				  params in
	let new_expr1 = g2 expr1 (Env.add new_env name new_name) in
	let new_expr2 = g2 expr2 (Env.add env name new_name) in
	LetRec ((new_name, t), new_params, new_expr1, new_expr2)
  | Application (name, name_list) ->
	Application (Env.get2 env name,
	      List.map (fun var -> Env.get2 env var) name_list)


(* Alpha.f : Knormal.t -> Knormal.t *)

let f expr = g expr Env.empty_env
