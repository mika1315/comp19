{
(* 補助的な変数、関数、型などの定義 *)
open Parser
}

(* 正規表現の略記 *)
(* [...] の中は character '...' でなくてはならない *)
let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper

rule token = parse
| space+ { token lexbuf }       (* スペースは読み飛ばす *)
| "(*" [^ '\n']* "\n"           (* ( * から行末まではコメント *)
	 { token lexbuf }
| "("	 { LPAREN }
| ")"	 { RPAREN }
| "+"    { PLUS }
| "-"	 { MINUS }
| "*"	 { TIMES }
| "/"    { DIVIDE }
| "mod"  { MOD }
| "+."   { PLUSDOT }
| "-."	 { MINUSDOT }
| "*."	 { TIMESDOT }
| "/."   { DIVIDEDOT }
| "let"  { LET }
| "rec"  { REC }
| "in"   { IN }
| "if"   { IF }
| "then" { THEN }
| "else" { ELSE }
| "="    { EQUAL }
| "<>"   { NOTEQUAL }
| "<="   { LESSEQUAL }
| ">="   { GREATEREQUAL }
| "<"    { LESS }
| ">"    { GREATER }
| digit+                        (* 整数 *)
         { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
| digit+ "." digit*             (* 実数 *)
	 { REAL (float_of_string (Lexing.lexeme lexbuf)) }
| lower+ (alpha* | digit*)*     (* 変数：アルファベットで始まりアルファベットまたは数字が続いたもの *)
         { VARIABLE (Lexing.lexeme lexbuf) }
| eof	 { EOF }                (* 入力終了 *)
| _	 { failwith ("unknown token: " ^ Lexing.lexeme lexbuf) }
