(* ユーザプログラムの型を表す型 *)
type t = Int
       | Float
       | Fun of t list * t    (* t list が引数の型のリスト、t が返り値の型 *)
       | TVar of t option ref (* 型変数 *)

(* 新しい型変数を作る *)
(* Type.gen_type : unit -> Type.t *)
let gen_type () = TVar (ref None)

(* Type.print: 型をプリントする関数（デバッグ用）  *)
let rec to_string t = match t with
    Int -> "int"
  | Float -> "float"
  | Fun ([], t) -> failwith "argument type empty"
  | Fun (t :: ts, t') ->
	"("
	^ List.fold_left (fun str arg -> str ^ " -> " ^ to_string arg)
			 (to_string t)
			 ts
	^ " -> "
	^ to_string t'
	^ ")"
  | TVar (r) -> failwith "type variable found"

let print t =
  let str = to_string t
  in (print_string str;
      print_newline ())
