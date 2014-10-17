type token =
  | INT of (int)
  | VAR of (string)
  | TEST of (Ast.info)
  | PLUS
  | MINUS
  | TIMES
  | EOF
  | LPAREN
  | RPAREN
  | TRUE
  | FALSE
  | EQUALS
  | NOTEQUALS
  | LESS
  | LESSEQ
  | GREATER
  | GREATEREQ
  | FORALL
  | EXISTS
  | NOT
  | AND
  | OR
  | IMPLIES
  | DOLLAR
  | SKIP
  | ASSIGN
  | PRINT
  | SEMI
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO
  | LBRACE
  | RBRACE
  | AT

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.assn * Ast.com * Ast.assn
