(* Gensym.f : string -> string *)

(* 引数 var の後ろに _n（n は counter の値）を付けた文字列を返す *)
(* 古い _n が既に付いていたら、それを外して付け直す *)

let counter = ref 0

let f var =
  if var = "_" then var
  else (counter := !counter + 1;
	if String.contains var '_'
	then String.sub var 0 (String.index var '_')
		 ^ "_" ^ string_of_int !counter
	else var ^ "_" ^ string_of_int !counter)
