(* 環境：
   変数 key の値が value であることを保持するテーブル *)

(* key が見つからなかったときに raise される例外 *)
exception UnboundVariable of string

(* 空の環境 *)
let empty_env = []

(* 環境 env に (key, value) を加える *)
let add env key value = (key, value) :: env

(* 環境 env から key に対応する値を取り出す *)
let rec get env key = match env with
    [] -> raise (UnboundVariable key)
  | (first_key, first_value) :: rest ->
	if key = first_key then first_value
			   else get rest key
