# About this complier
Implementation of the compiler using OCaml that outputs Intel's machine language.

## Process
```mermaid
graph LR
A[Lexical analysis] --> B[Parsing]
B --> C[Type inference]
C --> D[Optimization]
D --> E[Register allocation]
E --> F[Generate the codes]
 ```

- Lexical analysis
The process of converting a sequence of characters (program written by users) into a sequence of tokens
- Parsing
The process of converting a sequence of tokens into a parse tree
- Type inference
The detection of the type each subexpression
- Optimization
The process of converting an input program (a parse tree representing the input program) to a "better" program (a parse tree representing the "better" program).
- Register allocation
The allocation of registers to the variables
- Generate the codes
Output the machine languages that correspond to the input program.
