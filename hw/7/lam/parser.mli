type token =
  | INT of (int)
  | VAR of (string)
  | PLUS
  | LPAREN
  | RPAREN
  | COMMA
  | DOT
  | EQUALS
  | WRITE
  | READ
  | REF
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | FST
  | SND
  | LAMBDA
  | LET
  | IN
  | EOF
  | AND

val exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.exp
