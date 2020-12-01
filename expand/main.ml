(* β変換から定数伝播までを変化がなくなるまで繰り返す *)
(* simple_optimize : Knormal.t -> Knormal.t *)
let rec simple_optimize kprogram0 =
  let kprogram = Beta.f kprogram0 in		(* β変換 *)
  let kprogram = Eta.f kprogram in		(* η変換 *)
  let kprogram = Assoc.f kprogram in		(* 結合性変換 *)
  let kprogram = Elim.f kprogram in		(* 不要変数除去 *)
  let kprogram = Constf.f kprogram in		(* 定数伝播 *)
  if kprogram = kprogram0 then kprogram
			  else simple_optimize kprogram

(* 各種最適化を n 回、行う *)
(* optimize : Knormal.t -> int -> Knormal.t *)
let rec optimize kprogram0 n =
  let kprogram = simple_optimize kprogram0 in
  if n > 0 then let kprogram = Expand.f kprogram in (* 関数展開 *)
                (*
                print_string "関数展開";
                print_newline ();
                Knormal.print kprogram;
                *)
                let kprogram = simple_optimize kprogram in
		optimize kprogram (n - 1)
	   else kprogram

(* メイン関数 *)
let go () =
  let program = Parser.start Lexer.token (Lexing.from_channel stdin) in
					(* 入力を構文解析し、*)
  let kprogram = Knormal.f program in	(* k-正規形に変換し、*)
  let kprogram = Typing.f kprogram in	(* 型推論をし、*)
  (*
  print_string "型推論";
  print_newline ();
  Knormal.print kprogram;
*)
  let kprogram = Alpha.f kprogram in	(* α変換し、*)
  (*
  print_string "α変換";
  print_newline ();
  Knormal.print kprogram;
*)
  let kprogram = optimize kprogram 0 in	(* 各種最適化を施し、*)
  (*
  print_string "関数展開した後に最適化";
  print_newline ();
  Knormal.print kprogram;
*)
  let cprogram = Closure.f kprogram in	(* クロージャ変換を行い、*)
  (*
  print_newline ();
  print_string "closure変換";
  print_newline ();
  Closure.print cprogram;
*)
  let cprogram = Prealloc.f cprogram in	(* レジスタ割り当て前処理を行い、*)
  (*
  print_newline ();
  print_string "レジスタ割り当て前処理";
  Closure.print cprogram;
*)
  let cprogram = Alloc.f cprogram in	(* レジスタ割り当てを行い、*)
  (*
  print_newline ();
  print_string "レジスタ割り当て";
  Closure.print cprogram;
*)
  let asm_code = Code.f cprogram in	(* コード生成を行い、*)
  print_string asm_code			(* 表示する。*)

(* スタートアップ *)
let _ = go ()
