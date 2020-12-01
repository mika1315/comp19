(* メイン関数 *)
let go () =
  let program = Parser.start Lexer.token (Lexing.from_channel stdin) in
					(* 入力を構文解析し、*)
  Syntax.print program			(* 表示する。*)

(* スタートアップ *)
let _ = go ()
