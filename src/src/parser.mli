type token =
  | VAR of (string)
  | ABST
  | OPEN
  | CLOSE
  | EOF
  | DOT

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Grammar.expr
