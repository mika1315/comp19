(* 定数伝播 *)

open Knormal

(* メイン *)
(* k-正規形に対する定数伝播 C。
ρ は、変数の値が(明らかに)確定している時に、その値を返す関数。
   確定していない場合は、変数をそのまま返す。*)

let rec c expr env = try (
  match expr with
    Number (num) -> expr
  | Real (f) -> expr
  | Variable (name) -> Variable (Env.get env name)
  | Op (name1, op, name2) ->
    (match op with
       Operator.Plus ->
       Number ((int_of_string (Env.get env name1)) + (int_of_string (Env.get env name2)))
     | Operator.Minus ->
       Number ((int_of_string (Env.get env name1)) - (int_of_string (Env.get env name2)))
     | Operator.Times ->
       Number ((int_of_string (Env.get env name1)) * (int_of_string (Env.get env name2)))
     | Operator.Divide ->
       Number ((int_of_string (Env.get env name1)) / (int_of_string (Env.get env name2)))
     | Operator.Mod ->
       Number ((int_of_string (Env.get env name1)) mod (int_of_string (Env.get env name2)))
     | Operator.PlusDot ->
       Real ((float_of_string (Env.get env name1)) +. (float_of_string (Env.get env name2)))
     | Operator.MinusDot ->
       Real ((float_of_string (Env.get env name1)) -. (float_of_string (Env.get env name2)))
     | Operator.TimesDot ->
       Real ((float_of_string (Env.get env name1)) *. (float_of_string (Env.get env name2)))
     | Operator.DivideDot ->
       Real ((float_of_string (Env.get env name1)) /. (float_of_string (Env.get env name2)))
    )
  | IfEqual (name1, name2, expr3, expr4) ->
    if (Env.get env name1) = (Env.get env name2)
    then c expr3 env
    else c expr4 env
  | IfLess (name1, name2, expr3, expr4) ->
    if (Env.get env name1) < (Env.get env name2)
    then c expr3 env
    else c expr4 env
  | Let ((name, t), expr1, expr2) ->
    (
    match expr1 with
    | Number (num) -> 
      let new_env = Env.add env name (string_of_int num) in
      let new_expr2 = c expr2 new_env in
      Let ((name, t), expr1, new_expr2)
    | Real (f) ->
      let new_env = Env.add env name (string_of_float f) in
      let new_expr2 = c expr2 new_env in
      Let ((name, t), expr1, new_expr2)
    | _->
      let new_expr1 = c expr1 env in
      let new_expr2 = c expr2 env in
      Let ((name, t), new_expr1, new_expr2)
  )
  | LetRec ((name, t), params, expr1, expr2) ->
    let new_expr1 = c expr1 env in
    let new_expr2 = c expr2 env in
    LetRec ((name, t), params, new_expr1, new_expr2)
  | Application (name, name_list) -> expr
)
  with Env.UnboundVariable variable -> match expr with
    | IfEqual (name1, name2, expr3, expr4) ->
      let new_expr3 = c expr3 env in
      let new_expr4 = c expr4 env in
      IfEqual (name1, name2, new_expr3, new_expr4)
    | IfLess (name1, name2, expr3, expr4) ->
      let new_expr3 = c expr3 env in
      let new_expr4 = c expr4 env in
      IfLess (name1, name2, new_expr3, new_expr4)
    | _ -> expr

let f expr = c expr Env.empty_env
