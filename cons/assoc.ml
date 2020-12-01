(* let の結合性変換 *)

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
  | Let ((name1, t1), Let ((name2, t2), expr3, expr4), expr5) ->
    let new_expr3 = g expr3 env in
    let new_expr4 = g expr4 env in
    let new_expr5 = g expr5 env in
    Let ((name2, t2), new_expr3, (Let ((name1, t1), new_expr4, new_expr5)))
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
