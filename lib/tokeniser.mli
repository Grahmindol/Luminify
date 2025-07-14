type token =
    Whitespace of string
  | Comment of string
  | StringStart of string
  | StringEnd of string
  | String of string
  | Keyword of string
  | Value of string
  | Ident of string
  | Number of string
  | Symbol of string
  | VarArg
  | Operator of string
  | LabelStart
  | LabelEnd
  | UnIdentified of string

val token_to_string : token -> string

val tokenise : string -> token list
val untokenise :  token list -> string

val filter_useless_tokens :  token list -> token list
