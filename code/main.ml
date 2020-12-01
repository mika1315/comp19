(* メイン関数 *)
let go () =
  let program = Parser.start Lexer.token (Lexing.from_channel stdin) in
					(* 入力を構文解析し、*)
  let kprogram = Knormal.f program in	(* k-正規形に変換し、*)
  let fprogram = First.f kprogram in	(* １階の言語に変換し、*)
  let fprogram = Prealloc.f fprogram in	(* レジスタ割り当て前処理を行い、*)
  let fprogram = Alloc.f fprogram in	(* レジスタ割り当てを行い、*)
  let asm_code = Code.f fprogram in	(* コード生成を行い、*)
  print_string asm_code			(* 表示する。*)

(* スタートアップ *)
let _ = go ()
