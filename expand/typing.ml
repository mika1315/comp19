open Knormal
open Env
    
(* 型を同じにできない場合に起きる例外 *)
exception Unify of Type.t * Type.t

(* 型エラー *)
exception Error of Knormal.t * Type.t * Type.t

(* 型変数を中身で置き換える。返ってくる型には型変数は含まれない *)
(* deref_type : Type.t -> Type.t *)
let rec deref_type t = match t with
    Type.Int -> Type.Int
  | Type.Float -> Type.Float
  | Type.Fun (ts, t') -> Type.Fun (List.map deref_type ts, deref_type t')
  | Type.TVar (r) -> match !r with
			None -> r := Some (Type.Int);
				     Type.Int
		      | Some (t') -> let t'' = deref_type t' in
				     r := Some (t'');
				     t''
(* (変数, 型) の型を deref する *)
(* deref_id_type : (string * Type.t) -> (string * Type.t) *)
let rec deref_id_type (x, t) = (x, deref_type t)

(* 項にでてくる型を deref する *)
(* deref_term : Knormal.t -> Knormal.t *)
let rec deref_term expr = match expr with
    Number (num) -> expr
  | Real (f) -> expr
  | Variable (name) -> Variable (name)
  | Op (arg1, op, arg2) -> expr
  | IfEqual (v1, v2, e3, e4) ->
      IfEqual (v1, v2, deref_term e3, deref_term e4)
  | IfLess (v1, v2, e3, e4) ->
      IfLess (v1, v2, deref_term e3, deref_term e4)
  | Let (xt, e1, e2) -> Let (deref_id_type xt, deref_term e1, deref_term e2)
  | LetRec (xt, params, e1, e2) ->
      LetRec (deref_id_type xt, List.map deref_id_type params,
	      deref_term e1, deref_term e2)
  | Application (e, es) -> expr

(* r が型 t に現れるかをチェックする (occur check) *)
(* occur : Type.t option ref -> Type.t -> bool *)
let rec occur r t = match t with
    Type.Int -> false
  | Type.Float -> false
  | Type.Fun (ts, t') -> List.exists (occur r) ts || occur r t'
  | Type.TVar (r') ->
      if r == r' then true else
	match !r' with
	    None -> false
	  | Some (t') -> occur r t'

(* t1 = t2 となるように、型変数への代入をする *)
(* unify : Type.t -> Type.t -> unit *)
let rec unify t1 t2 = match (t1, t2) with
    (Type.Int, Type.Int) -> ()
  | (Type.Float, Type.Float) -> ()
  | (Type.Fun (t1s, t1'), Type.Fun (t2s, t2')) ->
      (try List.iter2 unify t1s t2s with
         Invalid_argument ("List.iter2") -> raise (Unify (t1, t2)));
      unify t1' t2'
  | (Type.TVar (r1), Type.TVar (r2)) when r1 == r2 -> ()
  | (Type.TVar ({ contents = Some(t1') }), _) -> unify t1' t2
  | (_, Type.TVar ({ contents = Some(t2') })) -> unify t1 t2'
  | (Type.TVar ({ contents = None } as r1), _) -> (* t2 は TVar とは限らない *)
      if occur r1 t2 then raise (Unify (t1, t2))
		     else r1 := Some (t2)
  | (_, Type.TVar ({ contents = None } as r2)) -> (* t1 は TVar とは限らない *)
      if occur r2 t1 then raise (Unify (t1, t2))
		     else r2 := Some (t1)
  | (_, _) -> raise (Unify (t1, t2))

(* 型推論 *)
(* g : Knormal.t -> (string * Type.t) list -> Type.t *)
let rec g expr env = try (match expr with
      Number (num) -> Type.Int
    | Real (f) -> Type.Float
    | Variable (name) -> Env.get env name
    | Op (name1, op, name2) ->
      (match op with
	 Operator.Plus ->
         let t1 = g (Variable (name1)) env in
         unify Type.Int t1;
         let t2 = g (Variable (name2)) env in
         unify Type.Int t2;
         Type.Int
       | Operator.Minus ->
         let t1 = g (Variable (name1)) env in
         unify Type.Int t1;
         let t2 = g (Variable (name2)) env in
         unify Type.Int t2;
         Type.Int
       | Operator.Times ->
         let t1 = g (Variable (name1)) env in
         unify Type.Int t1;
         let t2 = g (Variable (name2)) env in
         unify Type.Int t2;
         Type.Int
       | Operator.Divide ->
         let t1 = g (Variable (name1)) env in
         unify Type.Int t1;
         let t2 = g (Variable (name2)) env in
         unify Type.Int t2;
         Type.Int
       | Operator.Mod ->
         let t1 = g (Variable (name1)) env in
         unify Type.Int t1;
         let t2 = g (Variable (name2)) env in
         unify Type.Int t2;
         Type.Int
       | Operator.PlusDot ->
         let t1 = g (Variable (name1)) env in
         unify Type.Float t1;
         let t2 = g (Variable (name2)) env in
         unify Type.Float t2;
         Type.Float
       | Operator.MinusDot ->
         let t1 = g (Variable (name1)) env in
         unify Type.Float t1;
         let t2 = g (Variable (name2)) env in
         unify Type.Float t2;
         Type.Float
       | Operator.TimesDot ->
         let t1 = g (Variable (name1)) env in
         unify Type.Float t1;
         let t2 = g (Variable (name2)) env in
         unify Type.Float t2;
         Type.Float
       | Operator.DivideDot ->
         let t1 = g (Variable (name1)) env in
         unify Type.Float t1;
         let t2 = g (Variable (name2)) env in
         unify Type.Float t2;
         Type.Float)
    | IfEqual (name1, name2, arg3, arg4) ->
      let t1 = g (Variable (name1)) env in
      let t2 = g (Variable (name2)) env in
      unify t1 t2;
      let t3 = g arg3 env in
      let t4 = g arg4 env in
      unify t3 t4;
      t3
    | IfLess (name1, name2, arg3, arg4) ->
      let t1 = g (Variable (name1)) env in
      let t2 = g (Variable (name2)) env in
      unify t1 t2;
      let t3 = g arg3 env in
      let t4 = g arg4 env in
      unify t3 t4;
      t3
    | Let ((name, t), arg1, arg2) ->
      let t1 = g arg1 env in
      unify t t1;
      let t2 = g arg2 (Env.add env name t) in
      t2
    | LetRec ((name, t), params, arg1, arg2) ->
      let f_env = Env.add env name t in
      let new_env = params @ f_env in
      let t_lst = List.map (fun (xi, ti) -> ti) params in
      let t1 = g arg1 new_env in
      unify (Type.Fun (t_lst, t1)) t;
      let t2 = g arg2 f_env in
      t2
    | Application (name, args) ->
      let t = Type.gen_type () in
      let ts = List.map (fun arg -> g (Variable (arg)) env) args in
      unify (Type.Fun (ts, t)) (g (Variable (name)) env);
      t
  )
  with
    Unify (t1, t2) ->
    Knormal.print (deref_term expr);
    Type.print (deref_type t1);
    Type.print (deref_type t2);
    raise (Error (deref_term expr, deref_type t1, deref_type t2))
  | Env.UnboundVariable (name) -> Type.gen_type ()

(* 型推論の入り口 *)
(* Typing.f : Knormal.t -> Knormal.t *)
let f expr =
  (try unify Type.Int (g expr Env.empty_env) with
   Unify _ -> failwith "top level does not have type int");
  deref_term expr
