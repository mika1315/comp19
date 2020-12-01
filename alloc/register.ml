(* i を受け取り _R_i を作る *)
(* make_register : int -> string *)
let make_register i = "_R_" ^ (string_of_int i)

(* j を受け取り _F_j を作る *)
(* make_fregister : int -> string *)
let make_fregister j = "_F_" ^ (string_of_int j)

(* 変数名がレジスタかどうかを判定する *)
(* 浮動小数点レジスタでも true を返す *)
(* is_register : string -> bool *)
let is_register r = String.get r 0 = '_'

(* 変数名が浮動小数点レジスタかどうかを判定する *)
(* is_fregister : string -> bool *)
let is_fregister f = String.get f 0 = '_' && String.get f 1 = 'F'
