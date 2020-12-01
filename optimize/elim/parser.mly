%{
    (* 補助的な変数、関数、型などの定義 *)
    open Syntax
    open Operator
    open Type
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token LPAREN
%token RPAREN
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token MOD
%token PLUSDOT
%token MINUSDOT
%token TIMESDOT
%token DIVIDEDOT
%token LET
%token REC
%token IN
%token IF
%token THEN
%token ELSE
%token EQUAL
%token NOTEQUAL
%token LESSEQUAL
%token GREATEREQUAL
%token LESS
%token GREATER
%token <int> NUMBER
%token <float> REAL
%token <string> VARIABLE
%token EOF
/* End of File: 入力の終わりを示す */

/* 非終端記号の型をここで宣言する */
%type <Syntax.t> start

/* 開始記号の定義 */
%start start

/* 演算子の優先順位を指定する */
/* 下に行くほど強く結合する */
%nonassoc ELSE IN
%left PLUS MINUS PLUSDOT MINUSDOT
%left TIMES DIVIDE MOD TIMESDOT DIVIDEDOT
/* nonassoc は結合なし（毎回、かっこを書かなくてはならない）、
   left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

start:
| expr
        { $1 }

simple_expr:
| NUMBER
        { Syntax.Number ($1) }
| REAL
        { Syntax.Real ($1) }
| VARIABLE
        { Syntax.Variable ($1) }
| LPAREN expr RPAREN
	{ $2 }

expr:
| simple_expr
        { $1 }
| expr PLUS expr
	{ Syntax.Op ($1, Operator.Plus, $3) }
| expr MINUS expr
	{ Syntax.Op ($1, Operator.Minus, $3) }
| expr TIMES expr
        { Syntax.Op ($1, Operator.Times, $3) }
| expr DIVIDE expr
        { Syntax.Op ($1, Operator.Divide, $3) }
| expr MOD expr
	{ Syntax.Op ($1, Operator.Mod, $3) }
| expr PLUSDOT expr
        { Syntax.Op ($1, Operator.PlusDot, $3) }
| expr MINUSDOT expr
        { Syntax.Op ($1, Operator.MinusDot, $3) }
| expr TIMESDOT expr
        { Syntax.Op ($1, Operator.TimesDot, $3) }
| expr DIVIDEDOT expr
        { Syntax.Op ($1, Operator.DivideDot, $3) }
| IF expr EQUAL expr THEN expr ELSE expr
        { Syntax.IfEqual ($2, $4, $6, $8) }
| IF expr NOTEQUAL expr THEN expr ELSE expr
        { Syntax.IfEqual ($2, $4, $8, $6) }
| IF expr LESSEQUAL expr THEN expr ELSE expr
        { Syntax.IfLess ($4, $2, $8, $6) }
| IF expr GREATEREQUAL expr THEN expr ELSE expr
        { Syntax.IfLess ($2, $4, $8, $6) }
| IF expr LESS expr THEN expr ELSE expr
        { Syntax.IfLess ($2, $4, $6, $8) }
| IF expr GREATER expr THEN expr ELSE expr
        { Syntax.IfLess ($4, $2, $6, $8) }
| LET VARIABLE EQUAL expr IN expr
        { Syntax.Let (($2, Type.gen_type()), $4, $6) }
| LET REC VARIABLE variables EQUAL expr IN expr
        { Syntax.LetRec (($3, Type.gen_type()), $4, $6, $8) }
| simple_expr app
        { Syntax.Application ($1, $2)}

variables:
| VARIABLE
        { [($1, Type.gen_type())] }
| VARIABLE variables
        { ($1, Type.gen_type()) :: $2 }

app:
| simple_expr
        { [$1] }
| simple_expr app
        { $1 :: $2 }
