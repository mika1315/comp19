(* β変換から定数伝播までを変化がなくなるまで繰り返す *)
(* simple_optimize : Knormal.t -> Knormal.t *)
let rec simple_optimize kprogram0 =
  let kprogram = kprogram0 in
  let kprogram = Beta.f kprogram in		(* β変換 *)
  (* let kprogram = Eta.f kprogram in *)		(* η変換 *)
  (* let kprogram = Assoc.f kprogram in *)		(* 結合性変換 *)
  (* let kprogram = Elim.f kprogram in *)		(* 不要変数除去 *)
  (* let kprogram = Constf.f kprogram in *)		(* 定数伝播 *)
  if kprogram = kprogram0 then kprogram
			  else simple_optimize kprogram

(* 各種最適化を n 回、行う *)
(* optimize : Knormal.t -> int -> Knormal.t *)
let rec optimize kprogram0 n =
  let kprogram = simple_optimize kprogram0 in
  if n > 0 then (* let kprogram = Expand.f kprogram in *) (* 関数展開 *)
		let kprogram = simple_optimize kprogram in
		optimize kprogram (n - 1)
	   else kprogram

(* メイン関数 *)
let go () =
  let program = Parser.start Lexer.token (Lexing.from_channel stdin) in
					(* 入力を構文解析し、*)
  let kprogram = Knormal.f program in	(* k-正規形に変換し、*)
  let kprogram = Alpha.f kprogram in	(* α変換し、*)
  let kprogram = optimize kprogram 0 in	(* 各種最適化を施し、*)
  (* Knormal.print kprogram *)		(* 表示する。*)
  let fprogram = First.f kprogram in	(* １階の言語に変換し、*)
  let fprogram = Prealloc.f fprogram in	(* レジスタ割り当て前処理を行い、*)
  let fprogram = Alloc.f fprogram in	(* レジスタ割り当てを行い、*)
  let asm_code = Code.f fprogram in	(* コード生成を行い、*)
  print_string asm_code			(* 表示する。*)

(* スタートアップ *)
let _ = go ()
