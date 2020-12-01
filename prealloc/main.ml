(* メイン関数 *)
let go () =
  let program = Parser.start Lexer.token (Lexing.from_channel stdin) in
					(* 入力を構文解析し、*)
  let kprogram = Knormal.f program in	(* k-正規形に変換し、*)
  let fprogram = First.f kprogram in	(* １階の言語に変換し、*)
  let fprogram = Prealloc.f fprogram in	(* レジスタ割り当て前処理を行い、*)
  First.print fprogram			(* 表示する。*)

(* スタートアップ *)
let _ = go ()
