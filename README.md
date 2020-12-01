# About this complier
Implementation of the compiler using OCaml that outputs Intel's machine language.

## Process
![スクリーンショット 2020-12-01 22 49 39](https://user-images.githubusercontent.com/51024485/100749123-9306d700-3427-11eb-9570-72bbd0044616.png)

- Lexical analysis

The process of converting a sequence of characters (program written by users) into a sequence of tokens


- Parsing

The process of converting a sequence of tokens into a parse tree


- Type inference

The detection of the type each subexpression


- Optimization

The process of converting an input program (a parse tree representing the input program) to a "better" program (a parse tree representing the "better" program)


- Register allocation

The allocation of registers to the variables


- Generate the codes

Output the machine languages that correspond to the input program
