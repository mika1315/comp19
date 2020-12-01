(* 不要変数除去 *)

open Knormal

(* 変数 x が k-正規形の式 E の中で使われているかを判定する関数 U(x)[E] *)
let rec used vari expr = match expr with
    Number (num) -> false
  | Real (f) -> false
  | Variable (name) -> if name = vari then true else false
  | Op (name1, op, name2) ->
    if (name1 = vari) || (name2 = vari)
    then true
    else false
  | IfEqual (name1, name2, expr3, expr4) ->
    if (name1 = vari) || (name2 = vari) ||
       (used vari expr3) || (used vari expr4)
    then true
    else false
  | IfLess (name1, name2, expr3, expr4) ->
    if (name1 = vari) || (name2 = vari) ||
       (used vari expr3) || (used vari expr4)
    then true
    else false
  | Let ((name, t), expr1, expr2) ->
    if (used vari expr1) || ((name <> vari) && (used vari expr2))
    then true
    else false
  | LetRec ((name, t), params, expr1, expr2) ->
    if ((name <> vari) && (List.for_all (fun (v, t) -> v <> vari) params) && (used vari expr1)) || ((name <> vari) && (used vari expr2))
    then true
    else false
  | Application (name, name_list) ->
    if (name = vari) || (List.exists (fun v -> v = vari) name_list)
    then true
    else false
      
(* k-正規形の式 E が自明かを判定する関数 T [E] *)
let rec obvious expr = match expr with
    Number (num) -> true
  | Real (f) -> true
  | Variable (name) -> true
  | Op (name1, op, name2) -> true
  | IfEqual (name1, name2, expr3, expr4) -> false
  | IfLess (name1, name2, expr3, expr4) -> false
  | Let ((name, t), expr1, expr2) -> false
  | LetRec ((name, t), params, expr1, expr2) -> false
  | Application (name, name_list) -> false

(* メイン *)

let rec r expr = match expr with
    Number (num) -> expr
  | Real (f) -> expr
  | Variable (name) -> expr
  | Op (name1, op, name2) -> expr
  | IfEqual (name1, name2, expr3, expr4) ->
    IfEqual (name1, name2, r expr3, r expr4)
  | IfLess (name1, name2, expr3, expr4) ->
    IfLess (name1, name2, r expr3, r expr4)
  | Let ((name, t), expr1, expr2) ->
    let new_expr1 = r expr1 in
    let new_expr2 = r expr2 in
    if (obvious new_expr1) = true && (used name new_expr2) = false
    then new_expr2
    else Let ((name, t), new_expr1, new_expr2)
  | LetRec ((name, t), params, expr1, expr2) ->
    let new_expr1 = r expr1 in
    let new_expr2 = r expr2 in
    if (used name new_expr2) = false
    then new_expr2
    else LetRec ((name, t), params, new_expr1, new_expr2)
  | Application (name, name_list) ->
    expr

(* Elim.f : Knormal.t -> Knormal.t *)

let f expr = r expr
