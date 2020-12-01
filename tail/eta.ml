(* η変換 *)

open Knormal

(* メイン *)

let rec g expr env = match expr with
    Number (num) -> expr
  | Real (f) -> expr
  | Variable (name) -> expr
  | Op (name1, op, name2) -> expr
  | IfEqual (name1, name2, expr3, expr4) ->
    let new_expr3 = g expr3 env in
    let new_expr4 = g expr4 env in
    IfEqual (name1, name2, new_expr3, new_expr4)
  | IfLess (name1, name2, expr3, expr4) ->
    let new_expr3 = g expr3 env in
    let new_expr4 = g expr4 env in
    IfLess (name1, name2, new_expr3, new_expr4)
  | Let ((name, t), expr1, Variable (val_name)) ->
    if name = val_name then g expr1 env
    else
      let new_expr1 = g expr1 env in
      let new_expr2 = g (Variable (val_name)) env in
      Let ((name, t), new_expr1, new_expr2)
  | Let ((name, t), expr1, expr2) -> 
    let new_expr1 = g expr1 env in
    let new_expr2 = g expr2 env in
    Let ((name, t), new_expr1, new_expr2)
  | LetRec ((name, t), params, expr1, expr2) ->
    let new_expr1 = g expr1 env in
    let new_expr2 = g expr2 env in
    LetRec ((name, t), params, new_expr1, new_expr2)
  | Application (name, name_list) -> expr

(* Beta.f : Knormal.t -> Knormal.t *)

let f expr = g expr Env.empty_env
