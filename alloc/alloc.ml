(* 単純なレジスタ割り当て *)

open First
open Register

(* Set モジュール *)
(* (変数 × 変数) の集合　*)
module ReSet = Set.Make (struct type t = (string * string)
    let compare = compare
  end)

(* 変数の集合 *)
module ValSet = Set.Make (String)

(* 式 expr の中の自由変数を返す関数 *)
(* Code.free_v : First.t -> Set.t (string) *)
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
  | Application (name, register_list) ->
    List.fold_right ValSet.add register_list ValSet.empty

(* 「同じレジスタに割り当てたい」変数の組の集合の計算 *)
(* string -> First.t -> Set.t (string * stirng) *)
let rec target vari expr = match expr with
    Number (num) -> ReSet.empty
  | Real (f) -> ReSet.empty
  | Variable (name) -> ReSet.singleton (vari, name)
  | Op (name1, op, name2) -> ReSet.empty
  | IfEqual (name1, name2, expr3, expr4) ->
    ReSet.union (target vari expr3) (target vari expr4)
  | IfLess (name1, name2, expr3, expr4) ->
    ReSet.union (target vari expr3) (target vari expr4)
  | Let ((name, t), expr1, expr2) ->
    ReSet.union (target name expr1) (target vari expr2)
  | Application (name, name_list) ->
    ReSet.singleton (vari, make_register 0)

(* 「同じレジスタに割り当ててはいけない」変数の組の集合の計算 *)
(* Set.t (string) -> First.t -> Set.t (string * string) *)
let rec interfere live expr = match expr with
    Number (num) -> ReSet.empty
  | Real (f) -> ReSet.empty
  | Variable (name) -> ReSet.empty
  | Op (name1, op, name2) -> ReSet.empty
  | IfEqual (name1, name2, expr3, expr4) ->
    ReSet.union (interfere live expr3) (interfere live expr4)
  | IfLess (name1, name2, expr3, expr4) ->
    ReSet.union (interfere live expr3) (interfere live expr4)
  | Let ((name, t), expr1, expr2) ->
    let new_live = ValSet.union live (ValSet.remove name (free_v expr2)) in
    let live_pair =
      ValSet.fold (fun a set -> (ReSet.add (name, a) set)) new_live ReSet.empty in
    ReSet.union (interfere new_live expr1)
      (ReSet.union live_pair (interfere live expr2))
  | Application (name, name_list) -> ReSet.empty

exception Empty of ValSet.t
(* 適切なレジスタを返す関数 *)
(* string -> (string -> string) -> Set.t (string * string) -> Set.t (string * string) -> string *)
let rec allocone vari env t i register_set =
  if is_register vari
  then vari
  else
    let bad_together_p =
      ReSet.filter (fun (a, b) -> if (a = vari) || (b = vari)
                     then true
                     else false) i in
    (* ReSet.iter (fun (a, b) -> print_string (a ^ "と" ^ b); print_newline()) bad_together_p; *)
    let bad_together = 
      ReSet.fold (fun (a, b) set -> if (a = vari)
                   then ValSet.add b set
                   else ValSet.add a set) bad_together_p ValSet.empty in
    (* ValSet.iter (fun a -> print_string vari;
                  print_string ("は" ^ a ^ "と同じはダメ");
                  print_newline()) bad_together; *)
    (* 使いたくないレジスタの集合 *)
    let bad_register = 
      ValSet.fold (fun v set -> try
                      if is_register v
                      then ValSet.add (Env.get (Env.add env v v)  v) set
                      else ValSet.add (Env.get env v) set
                    with Env.UnboundVariable v ->
                      ValSet.union ValSet.empty set) bad_together ValSet.empty in
    (* ValSet.iter (fun a -> print_string (vari ^ "は" ^ a ^ "を使っちゃダメ");
                  print_newline()) bad_register; *)
    let good_together_p =
      ReSet.filter (fun (a, b) -> if (a = vari) || (b = vari)
                     then true
                     else false) t in
    let good_together = 
      ReSet.fold (fun (a, b) set -> if (a = vari)
                   then ValSet.add b set
                   else ValSet.add a set) good_together_p ValSet.empty in
    (* 使いたいレジスタの集合 *)
    let good_register =
      ValSet.fold (fun v set -> try
                      if is_register v
                      then ValSet.add (Env.get (Env.add env v v)  v) set
                      else ValSet.add (Env.get env v) set
                    with Env.UnboundVariable v ->
                      ValSet.union ValSet.empty set) good_together ValSet.empty in
    (* とりあえず使えるレジスタの集合 *)
    let better_register =
      ValSet.diff register_set bad_register in
    (* ValSet.iter (fun a ->
        print_string vari;
        print_string " : better : ";
                  print_string a;
                print_newline();) better_register; *)
    let best_register =
      ValSet.inter better_register good_register in
    (* ValSet.iter (fun a ->
        print_string vari;
        print_string " : best : ";
                  print_string a;
                print_newline();) best_register; *)
    (* 使いたいレジスタがあって、使えたらその中から使う *)
    if ValSet.cardinal best_register <> 0
    then
        ValSet.choose best_register
    else
        ValSet.choose better_register
    
(* メイン *)
(* First.t -> (string -> string) -> Set.t (string * string) -> Set.t (string * string) -> First.t *)
let rec a_expr expr env tg i register_set =
  match expr with
    Number (num) -> expr
  | Real (f) -> expr
  | Variable (name) -> Variable (Env.get env name)
  | Op (name1, op, name2) -> Op (Env.get env name1, op, Env.get env name2)
  | IfEqual (name1, name2, expr3, expr4) ->
    let new_expr3 = a_expr expr3 env tg i register_set in
    let new_expr4 = a_expr expr4 env tg i register_set in
    IfEqual (Env.get env name1, Env.get env name2, new_expr3, new_expr4)
  | IfLess (name1, name2, expr3, expr4) ->
    let new_expr3 = a_expr expr3 env tg i register_set in
    let new_expr4 = a_expr expr4 env tg i register_set in
    IfLess (Env.get env name1, Env.get env name2, new_expr3, new_expr4)
  | Let ((name, t), expr1, expr2) ->
    let new_register = allocone name env tg i register_set in
    let new_expr1 = a_expr expr1 env tg i register_set in
    let new_expr2 = a_expr expr2 (Env.add env name new_register) tg i register_set in
    Let ((new_register, t), new_expr1, new_expr2)
  | Application (name, register_list) -> Application (name, register_list)

(* 使えるレジスタのリスト *)
let register = [make_register 0; make_register 1; make_register 2;
                make_register 3; make_register 4; make_register 5;
                make_register 6; make_register 7; make_register 8;
                make_register 9; make_register 10; make_register 11]

(* 使えるレジスタの集合 *)
let register_set = List.fold_right ValSet.add register ValSet.empty

let a_def (FunDef ((name, t), params, expr)) =
  let tg = target (make_register 0) expr in
  let i = interfere ValSet.empty expr in
  let env = List.map (fun (param, t) -> (param, param)) params in 
  FunDef ((name, t), params, (a_expr expr env tg i register_set))

let a_program (Program (def_list, expr)) =
  let tg = target (make_register 0) expr in
  let i = interfere ValSet.empty expr in
  Program (List.map a_def def_list, (a_expr expr Env.empty_env tg i register_set))

(* Alloc.f : First.prog_t -> First.prog_t *)

(* 単純なレジスタ割り当て *)
let f program = a_program program
