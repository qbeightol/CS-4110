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

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"

open Ast
open Printf
open Lexing
# 47 "parser.ml"
let yytransl_const = [|
  260 (* PLUS *);
  261 (* MINUS *);
  262 (* TIMES *);
    0 (* EOF *);
  263 (* LPAREN *);
  264 (* RPAREN *);
  265 (* TRUE *);
  266 (* FALSE *);
  267 (* EQUALS *);
  268 (* NOTEQUALS *);
  269 (* LESS *);
  270 (* LESSEQ *);
  271 (* GREATER *);
  272 (* GREATEREQ *);
  273 (* FORALL *);
  274 (* EXISTS *);
  275 (* NOT *);
  276 (* AND *);
  277 (* OR *);
  278 (* IMPLIES *);
  279 (* DOLLAR *);
  280 (* SKIP *);
  281 (* ASSIGN *);
  282 (* PRINT *);
  283 (* SEMI *);
  284 (* IF *);
  285 (* THEN *);
  286 (* ELSE *);
  287 (* WHILE *);
  288 (* DO *);
  289 (* LBRACE *);
  290 (* RBRACE *);
  291 (* AT *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* VAR *);
  259 (* TEST *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\005\000\005\000\006\000\006\000\006\000\
\007\000\007\000\007\000\008\000\008\000\009\000\009\000\009\000\
\009\000\003\000\003\000\010\000\010\000\011\000\011\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\013\000\013\000\013\000\013\000\001\000\001\000\001\000\015\000\
\015\000\015\000\016\000\016\000\014\000\017\000\017\000\018\000\
\018\000\019\000\019\000\020\000\020\000\021\000\021\000\021\000\
\021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
\004\000\000\000"

let yylen = "\002\000\
\003\000\003\000\001\000\003\000\001\000\001\000\001\000\003\000\
\003\000\003\000\001\000\003\000\001\000\001\000\001\000\002\000\
\003\000\003\000\001\000\003\000\001\000\002\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\001\000\003\000\002\000\002\000\004\000\003\000\001\000\006\000\
\005\000\001\000\001\000\003\000\003\000\003\000\001\000\003\000\
\001\000\003\000\001\000\002\000\001\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\006\000\006\000\003\000\
\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\066\000\000\000\014\000\015\000\000\000\
\054\000\055\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\000\000\000\000\000\000\051\000\053\000\000\000\000\000\
\033\000\000\000\000\000\000\000\000\000\000\000\043\000\039\000\
\042\000\000\000\000\000\000\000\000\000\052\000\016\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\045\000\000\000\000\000\000\000\006\000\007\000\000\000\
\024\000\025\000\000\000\000\000\000\000\000\000\005\000\000\000\
\021\000\023\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\017\000\064\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\012\000\000\000\
\000\000\050\000\000\000\000\000\000\000\022\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\044\000\038\000\000\000\065\000\
\000\000\000\000\000\000\008\000\032\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\004\000\020\000\
\000\000\000\000\037\000\000\000\000\000\000\000\041\000\062\000\
\063\000\040\000"

let yydgoto = "\002\000\
\030\000\060\000\061\000\004\000\062\000\063\000\015\000\016\000\
\017\000\064\000\065\000\066\000\031\000\005\000\032\000\033\000\
\018\000\019\000\020\000\021\000\022\000"

let yysindex = "\012\000\
\231\254\000\000\116\000\000\000\198\255\000\000\000\000\116\000\
\000\000\000\000\008\255\047\255\135\000\076\255\182\000\080\255\
\000\000\003\255\068\255\074\255\000\000\000\000\072\255\147\000\
\000\000\143\255\147\000\147\000\198\255\235\254\000\000\000\000\
\000\000\156\000\082\255\116\255\139\255\000\000\000\000\070\255\
\070\255\070\255\070\255\070\255\070\255\070\255\070\255\070\255\
\116\000\000\000\116\000\116\000\143\255\000\000\000\000\147\000\
\000\000\000\000\167\255\195\000\126\255\146\255\000\000\089\255\
\000\000\000\000\143\255\036\255\243\254\016\255\086\255\000\255\
\158\000\000\000\000\000\166\255\168\255\070\255\080\255\080\255\
\132\255\132\255\132\255\132\255\132\255\132\255\000\000\068\255\
\074\255\000\000\036\255\169\000\039\255\000\000\143\255\143\255\
\143\255\143\255\143\255\143\255\143\255\143\255\147\000\143\255\
\147\000\202\255\187\255\231\254\000\000\000\000\198\255\000\000\
\116\000\116\000\242\255\000\000\000\000\146\255\146\255\036\255\
\036\255\036\255\036\255\036\255\036\255\089\255\000\000\000\000\
\149\255\187\255\000\000\088\255\104\255\187\255\000\000\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\255\
\000\000\000\000\103\255\041\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\182\255\087\255\000\000\048\000\
\000\000\000\000\000\000\185\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\053\255\183\255\
\024\255\037\000\076\000\081\000\086\000\092\000\000\000\135\255\
\107\255\000\000\187\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\119\255\151\255\207\255\
\224\255\240\255\001\000\017\000\033\000\058\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\153\000\239\255\233\255\000\000\065\000\080\000\009\000\152\000\
\154\000\105\000\109\000\160\000\149\000\227\255\112\000\156\255\
\248\255\176\000\179\000\181\000\222\000"

let yytablesize = 478
let yytable = "\035\000\
\073\000\023\000\024\000\069\000\070\000\072\000\129\000\103\000\
\068\000\003\000\011\000\011\000\001\000\003\000\011\000\107\000\
\034\000\011\000\011\000\011\000\011\000\011\000\011\000\025\000\
\049\000\026\000\011\000\011\000\011\000\135\000\036\000\056\000\
\093\000\138\000\003\000\091\000\103\000\050\000\092\000\095\000\
\096\000\011\000\111\000\056\000\056\000\056\000\117\000\108\000\
\049\000\106\000\081\000\082\000\083\000\084\000\085\000\086\000\
\009\000\009\000\056\000\103\000\009\000\049\000\049\000\009\000\
\009\000\009\000\009\000\009\000\009\000\037\000\006\000\007\000\
\009\000\009\000\009\000\049\000\078\000\039\000\130\000\120\000\
\121\000\122\000\123\000\124\000\125\000\048\000\115\000\009\000\
\051\000\075\000\003\000\003\000\014\000\052\000\003\000\136\000\
\053\000\003\000\003\000\003\000\003\000\003\000\003\000\049\000\
\132\000\133\000\003\000\003\000\105\000\049\000\047\000\137\000\
\072\000\003\000\048\000\003\000\003\000\076\000\003\000\109\000\
\003\000\003\000\001\000\001\000\047\000\049\000\001\000\048\000\
\048\000\001\000\001\000\001\000\001\000\001\000\001\000\040\000\
\041\000\047\000\001\000\001\000\077\000\048\000\046\000\054\000\
\055\000\001\000\103\000\001\000\001\000\067\000\001\000\104\000\
\001\000\001\000\002\000\002\000\046\000\112\000\002\000\118\000\
\119\000\002\000\002\000\002\000\002\000\002\000\002\000\054\000\
\055\000\046\000\002\000\002\000\113\000\056\000\114\000\057\000\
\058\000\002\000\134\000\002\000\002\000\071\000\002\000\127\000\
\002\000\002\000\010\000\010\000\023\000\024\000\010\000\079\000\
\080\000\010\000\010\000\010\000\010\000\010\000\010\000\023\000\
\024\000\087\000\010\000\010\000\010\000\095\000\096\000\126\000\
\036\000\116\000\025\000\036\000\026\000\128\000\026\000\036\000\
\036\000\010\000\094\000\029\000\110\000\025\000\131\000\026\000\
\088\000\027\000\026\000\026\000\028\000\089\000\029\000\027\000\
\090\000\026\000\038\000\026\000\026\000\000\000\026\000\000\000\
\026\000\026\000\000\000\027\000\027\000\040\000\041\000\028\000\
\000\000\074\000\027\000\000\000\027\000\027\000\000\000\027\000\
\000\000\027\000\027\000\028\000\028\000\000\000\000\000\000\000\
\029\000\000\000\028\000\000\000\028\000\028\000\000\000\028\000\
\000\000\028\000\028\000\000\000\029\000\029\000\000\000\000\000\
\030\000\000\000\000\000\029\000\000\000\029\000\029\000\000\000\
\029\000\000\000\029\000\029\000\030\000\030\000\000\000\000\000\
\031\000\000\000\000\000\030\000\057\000\030\000\030\000\000\000\
\030\000\000\000\030\000\030\000\031\000\031\000\000\000\019\000\
\057\000\057\000\057\000\031\000\000\000\031\000\031\000\000\000\
\031\000\018\000\031\000\031\000\019\000\000\000\000\000\057\000\
\000\000\000\000\019\000\000\000\019\000\019\000\018\000\019\000\
\000\000\019\000\019\000\058\000\018\000\000\000\018\000\018\000\
\059\000\018\000\000\000\018\000\018\000\060\000\000\000\058\000\
\058\000\058\000\000\000\061\000\059\000\059\000\059\000\000\000\
\000\000\060\000\060\000\060\000\000\000\000\000\058\000\061\000\
\061\000\061\000\000\000\059\000\006\000\007\000\000\000\000\000\
\060\000\000\000\008\000\000\000\009\000\010\000\061\000\000\000\
\000\000\000\000\000\000\000\000\011\000\012\000\013\000\006\000\
\007\000\000\000\014\000\000\000\000\000\008\000\000\000\009\000\
\010\000\000\000\000\000\054\000\055\000\000\000\000\000\011\000\
\012\000\056\000\000\000\057\000\058\000\014\000\000\000\040\000\
\041\000\000\000\000\000\074\000\000\000\059\000\042\000\043\000\
\044\000\045\000\046\000\047\000\095\000\096\000\000\000\000\000\
\116\000\000\000\000\000\097\000\098\000\099\000\100\000\101\000\
\102\000\040\000\041\000\000\000\000\000\000\000\000\000\000\000\
\042\000\043\000\044\000\045\000\046\000\047\000\095\000\096\000\
\000\000\000\000\000\000\000\000\000\000\097\000\098\000\099\000\
\100\000\101\000\102\000\035\000\000\000\034\000\035\000\000\000\
\034\000\000\000\035\000\035\000\034\000\034\000"

let yycheck = "\008\000\
\030\000\002\001\003\001\027\000\028\000\027\001\107\000\021\001\
\026\000\035\001\004\001\005\001\001\000\035\001\008\001\029\001\
\008\000\011\001\012\001\013\001\014\001\015\001\016\001\024\001\
\022\001\026\001\020\001\021\001\022\001\130\000\023\001\008\001\
\056\000\134\000\035\001\053\000\021\001\035\001\056\000\004\001\
\005\001\035\001\072\000\020\001\021\001\022\001\008\001\032\001\
\008\001\067\000\042\000\043\000\044\000\045\000\046\000\047\000\
\004\001\005\001\035\001\021\001\008\001\021\001\022\001\011\001\
\012\001\013\001\014\001\015\001\016\001\023\001\001\001\002\001\
\020\001\021\001\022\001\035\001\007\001\002\001\108\000\097\000\
\098\000\099\000\100\000\101\000\102\000\006\001\078\000\035\001\
\021\001\008\001\004\001\005\001\023\001\020\001\008\001\008\001\
\025\001\011\001\012\001\013\001\014\001\015\001\016\001\022\001\
\113\000\114\000\020\001\021\001\020\001\022\001\008\001\008\001\
\027\001\027\001\008\001\029\001\030\001\002\001\032\001\034\001\
\034\001\035\001\004\001\005\001\022\001\022\001\008\001\021\001\
\022\001\011\001\012\001\013\001\014\001\015\001\016\001\004\001\
\005\001\035\001\020\001\021\001\002\001\035\001\008\001\001\001\
\002\001\027\001\021\001\029\001\030\001\007\001\032\001\006\001\
\034\001\035\001\004\001\005\001\022\001\000\000\008\001\095\000\
\096\000\011\001\012\001\013\001\014\001\015\001\016\001\001\001\
\002\001\035\001\020\001\021\001\007\001\007\001\007\001\009\001\
\010\001\027\001\030\001\029\001\030\001\029\000\032\001\104\000\
\034\001\035\001\004\001\005\001\002\001\003\001\008\001\040\000\
\041\000\011\001\012\001\013\001\014\001\015\001\016\001\002\001\
\003\001\048\000\020\001\021\001\022\001\004\001\005\001\103\000\
\027\001\008\001\024\001\030\001\026\001\105\000\008\001\034\001\
\035\001\035\001\059\000\033\001\072\000\024\001\111\000\026\001\
\049\000\028\001\020\001\021\001\031\001\051\000\033\001\008\001\
\052\000\027\001\013\000\029\001\030\001\255\255\032\001\255\255\
\034\001\035\001\255\255\020\001\021\001\004\001\005\001\008\001\
\255\255\008\001\027\001\255\255\029\001\030\001\255\255\032\001\
\255\255\034\001\035\001\020\001\021\001\255\255\255\255\255\255\
\008\001\255\255\027\001\255\255\029\001\030\001\255\255\032\001\
\255\255\034\001\035\001\255\255\020\001\021\001\255\255\255\255\
\008\001\255\255\255\255\027\001\255\255\029\001\030\001\255\255\
\032\001\255\255\034\001\035\001\020\001\021\001\255\255\255\255\
\008\001\255\255\255\255\027\001\008\001\029\001\030\001\255\255\
\032\001\255\255\034\001\035\001\020\001\021\001\255\255\008\001\
\020\001\021\001\022\001\027\001\255\255\029\001\030\001\255\255\
\032\001\008\001\034\001\035\001\021\001\255\255\255\255\035\001\
\255\255\255\255\027\001\255\255\029\001\030\001\021\001\032\001\
\255\255\034\001\035\001\008\001\027\001\255\255\029\001\030\001\
\008\001\032\001\255\255\034\001\035\001\008\001\255\255\020\001\
\021\001\022\001\255\255\008\001\020\001\021\001\022\001\255\255\
\255\255\020\001\021\001\022\001\255\255\255\255\035\001\020\001\
\021\001\022\001\255\255\035\001\001\001\002\001\255\255\255\255\
\035\001\255\255\007\001\255\255\009\001\010\001\035\001\255\255\
\255\255\255\255\255\255\255\255\017\001\018\001\019\001\001\001\
\002\001\255\255\023\001\255\255\255\255\007\001\255\255\009\001\
\010\001\255\255\255\255\001\001\002\001\255\255\255\255\017\001\
\018\001\007\001\255\255\009\001\010\001\023\001\255\255\004\001\
\005\001\255\255\255\255\008\001\255\255\019\001\011\001\012\001\
\013\001\014\001\015\001\016\001\004\001\005\001\255\255\255\255\
\008\001\255\255\255\255\011\001\012\001\013\001\014\001\015\001\
\016\001\004\001\005\001\255\255\255\255\255\255\255\255\255\255\
\011\001\012\001\013\001\014\001\015\001\016\001\004\001\005\001\
\255\255\255\255\255\255\255\255\255\255\011\001\012\001\013\001\
\014\001\015\001\016\001\027\001\255\255\027\001\030\001\255\255\
\030\001\255\255\034\001\035\001\034\001\035\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  EOF\000\
  LPAREN\000\
  RPAREN\000\
  TRUE\000\
  FALSE\000\
  EQUALS\000\
  NOTEQUALS\000\
  LESS\000\
  LESSEQ\000\
  GREATER\000\
  GREATEREQ\000\
  FORALL\000\
  EXISTS\000\
  NOT\000\
  AND\000\
  OR\000\
  IMPLIES\000\
  DOLLAR\000\
  SKIP\000\
  ASSIGN\000\
  PRINT\000\
  SEMI\000\
  IF\000\
  THEN\000\
  ELSE\000\
  WHILE\000\
  DO\000\
  LBRACE\000\
  RBRACE\000\
  AT\000\
  "

let yynames_block = "\
  INT\000\
  VAR\000\
  TEST\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'taexp) in
    Obj.repr(
# 25 "parser.mly"
                                         ( Plus(_1, _3) )
# 357 "parser.ml"
               : aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'taexp) in
    Obj.repr(
# 26 "parser.mly"
                                         ( Minus(_1, _3) )
# 365 "parser.ml"
               : aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'taexp) in
    Obj.repr(
# 27 "parser.mly"
                                         ( _1 )
# 372 "parser.ml"
               : aexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'taexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aaexp) in
    Obj.repr(
# 29 "parser.mly"
                                         ( Times(_1, _3) )
# 380 "parser.ml"
               : 'taexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aaexp) in
    Obj.repr(
# 30 "parser.mly"
                                         ( _1 )
# 387 "parser.ml"
               : 'taexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 32 "parser.mly"
                                         ( Int(_1) )
# 394 "parser.ml"
               : 'aaexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 33 "parser.mly"
                                         ( Var(_1) )
# 401 "parser.ml"
               : 'aaexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : aexp) in
    Obj.repr(
# 34 "parser.mly"
                                         ( _2 )
# 408 "parser.ml"
               : 'aaexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tlexp) in
    Obj.repr(
# 37 "parser.mly"
                                         ( LPlus(_1,_3) )
# 416 "parser.ml"
               : 'lexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tlexp) in
    Obj.repr(
# 38 "parser.mly"
                                         ( LMinus(_1,_3) )
# 424 "parser.ml"
               : 'lexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tlexp) in
    Obj.repr(
# 39 "parser.mly"
                                         ( _1 )
# 431 "parser.ml"
               : 'lexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tlexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'alexp) in
    Obj.repr(
# 41 "parser.mly"
                                         ( LTimes(_1,_3) )
# 439 "parser.ml"
               : 'tlexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'alexp) in
    Obj.repr(
# 42 "parser.mly"
                                         ( _1 )
# 446 "parser.ml"
               : 'tlexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 45 "parser.mly"
                                         ( LInt(_1) )
# 453 "parser.ml"
               : 'alexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 46 "parser.mly"
                                         ( LPVar(_1) )
# 460 "parser.ml"
               : 'alexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 47 "parser.mly"
                                         ( LLVar(_2) )
# 467 "parser.ml"
               : 'alexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'lexp) in
    Obj.repr(
# 48 "parser.mly"
                                         ( _2 )
# 474 "parser.ml"
               : 'alexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : bexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cbexp) in
    Obj.repr(
# 51 "parser.mly"
                                         ( Or(_1, _3) )
# 482 "parser.ml"
               : bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cbexp) in
    Obj.repr(
# 52 "parser.mly"
                                         ( _1 )
# 489 "parser.ml"
               : bexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cbexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'nbexp) in
    Obj.repr(
# 54 "parser.mly"
                                         ( And(_1, _3) )
# 497 "parser.ml"
               : 'cbexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'nbexp) in
    Obj.repr(
# 55 "parser.mly"
                                         ( _1 )
# 504 "parser.ml"
               : 'cbexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'abexp) in
    Obj.repr(
# 57 "parser.mly"
                                         ( Not(_2) )
# 511 "parser.ml"
               : 'nbexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'abexp) in
    Obj.repr(
# 58 "parser.mly"
                                         ( _1 )
# 518 "parser.ml"
               : 'nbexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
                                         ( True )
# 524 "parser.ml"
               : 'abexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
                                         ( False )
# 530 "parser.ml"
               : 'abexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : aexp) in
    Obj.repr(
# 62 "parser.mly"
                                         ( Equals(_1, _3) )
# 538 "parser.ml"
               : 'abexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : aexp) in
    Obj.repr(
# 63 "parser.mly"
                                         ( NotEquals(_1, _3) )
# 546 "parser.ml"
               : 'abexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : aexp) in
    Obj.repr(
# 64 "parser.mly"
                                         ( Less(_1, _3) )
# 554 "parser.ml"
               : 'abexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : aexp) in
    Obj.repr(
# 65 "parser.mly"
                                         ( LessEq(_1, _3) )
# 562 "parser.ml"
               : 'abexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : aexp) in
    Obj.repr(
# 66 "parser.mly"
                                         ( Greater(_1, _3) )
# 570 "parser.ml"
               : 'abexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : aexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : aexp) in
    Obj.repr(
# 67 "parser.mly"
                                         ( GreaterEq(_1, _3) )
# 578 "parser.ml"
               : 'abexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : bexp) in
    Obj.repr(
# 68 "parser.mly"
                                         ( _2 )
# 585 "parser.ml"
               : 'abexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                                         ( Skip )
# 591 "parser.ml"
               : 'scom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : aexp) in
    Obj.repr(
# 73 "parser.mly"
                                         ( Assign(_1,_3) )
# 599 "parser.ml"
               : 'scom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : aexp) in
    Obj.repr(
# 74 "parser.mly"
                                         ( Print(_2) )
# 606 "parser.ml"
               : 'scom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : bexp) in
    Obj.repr(
# 75 "parser.mly"
                                         ( Test(_1,_2) )
# 614 "parser.ml"
               : 'scom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : com) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'icom) in
    Obj.repr(
# 78 "parser.mly"
                                         ( Seq(_1,_3,_4) )
# 623 "parser.ml"
               : com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : com) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'scom) in
    Obj.repr(
# 79 "parser.mly"
                                         ( SeqSimple(_1,_3) )
# 631 "parser.ml"
               : com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'icom) in
    Obj.repr(
# 80 "parser.mly"
                                         ( _1 )
# 638 "parser.ml"
               : com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : bexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'acom) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'acom) in
    Obj.repr(
# 82 "parser.mly"
                                         ( If(_2,_4,_6) )
# 647 "parser.ml"
               : 'icom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : bexp) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'acom) in
    Obj.repr(
# 83 "parser.mly"
                                         ( While(_2,_4,_5) )
# 656 "parser.ml"
               : 'icom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'acom) in
    Obj.repr(
# 84 "parser.mly"
                                         ( _1 )
# 663 "parser.ml"
               : 'icom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'scom) in
    Obj.repr(
# 86 "parser.mly"
                                         ( Simple(_1) )
# 670 "parser.ml"
               : 'acom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : com) in
    Obj.repr(
# 87 "parser.mly"
                                         ( _2 )
# 677 "parser.ml"
               : 'acom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'assn) in
    Obj.repr(
# 89 "parser.mly"
                                         ( _2 )
# 684 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assn) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'dassn) in
    Obj.repr(
# 91 "parser.mly"
                                         ( AImplies(_1,_3) )
# 692 "parser.ml"
               : 'assn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'dassn) in
    Obj.repr(
# 92 "parser.mly"
                                         ( _1 )
# 699 "parser.ml"
               : 'assn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'dassn) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cassn) in
    Obj.repr(
# 94 "parser.mly"
                                         ( AOr(_1,_3) )
# 707 "parser.ml"
               : 'dassn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cassn) in
    Obj.repr(
# 95 "parser.mly"
                                         ( _1 )
# 714 "parser.ml"
               : 'dassn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cassn) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'nassn) in
    Obj.repr(
# 97 "parser.mly"
                                         ( AAnd(_1,_3) )
# 722 "parser.ml"
               : 'cassn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'nassn) in
    Obj.repr(
# 98 "parser.mly"
                                         ( _1 )
# 729 "parser.ml"
               : 'cassn))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'aassn) in
    Obj.repr(
# 100 "parser.mly"
                                         ( ANot(_2) )
# 736 "parser.ml"
               : 'nassn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aassn) in
    Obj.repr(
# 101 "parser.mly"
                                         ( _1 )
# 743 "parser.ml"
               : 'nassn))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
                                         ( ATrue )
# 749 "parser.ml"
               : 'aassn))
; (fun __caml_parser_env ->
    Obj.repr(
# 104 "parser.mly"
                                         ( AFalse )
# 755 "parser.ml"
               : 'aassn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lexp) in
    Obj.repr(
# 105 "parser.mly"
                                         ( AEquals(_1,_3) )
# 763 "parser.ml"
               : 'aassn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lexp) in
    Obj.repr(
# 106 "parser.mly"
                                         ( ANotEquals(_1,_3) )
# 771 "parser.ml"
               : 'aassn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lexp) in
    Obj.repr(
# 107 "parser.mly"
                                         ( ALess(_1,_3) )
# 779 "parser.ml"
               : 'aassn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lexp) in
    Obj.repr(
# 108 "parser.mly"
                                         ( ALessEq(_1,_3) )
# 787 "parser.ml"
               : 'aassn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lexp) in
    Obj.repr(
# 109 "parser.mly"
                                         ( AGreater(_1,_3) )
# 795 "parser.ml"
               : 'aassn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lexp) in
    Obj.repr(
# 110 "parser.mly"
                                         ( AGreaterEq(_1,_3) )
# 803 "parser.ml"
               : 'aassn))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assn) in
    Obj.repr(
# 111 "parser.mly"
                                         ( AForall(_3,_5) )
# 811 "parser.ml"
               : 'aassn))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assn) in
    Obj.repr(
# 112 "parser.mly"
                                         ( AExists(_3,_5) )
# 819 "parser.ml"
               : 'aassn))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'assn) in
    Obj.repr(
# 113 "parser.mly"
                                         ( _2 )
# 826 "parser.ml"
               : 'aassn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'spec) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : com) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 115 "parser.mly"
                            ( (_1,_2,_3) )
# 835 "parser.ml"
               : Ast.assn * Ast.com * Ast.assn))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.assn * Ast.com * Ast.assn)
