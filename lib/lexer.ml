(* lexer.ml *)
let tokenize s =
  String.split_on_char ' ' s
