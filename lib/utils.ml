(* utils.ml *)

let is_latin_or_underscore c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c = '_')
  || (c >= '0' && c <= '9');;

let is_whitespace = function
  | ' ' | '\n' | '\t' | '\r' -> true
  | _ -> false

let is_symbol = function
  | '"' | '\''
  | ',' | '{' | '}' | '[' | ']' | '(' | ')' | ';' | '.' | ':' | '='
  | '~' | '&' | '|' | '#' | '>' | '<' 
  | '+' | '-' | '*' | '/' | '^' | '%' -> true
  | _ -> false

let is_keyword = function 
  | "break" | "do" | "else" | "elseif" | "end"
  | "for" | "function" | "goto" | "if" | "in"
  | "local" | "repeat" | "return"
  | "then" | "until" | "while" -> true
  | _ -> false

let is_value = function
  | "false" | "nil" | "true" -> true
  | _ -> false

let is_charop = function
  | "~" | "&" | "|" | "#" | ">" | "<" 
  | "+" | "-" | "*" | "/" | "^" | "%" -> true
  | _ -> false

let is_lua_numeral (s : string) : bool =
  let re_decimal_int = Str.regexp "^[0-9]+$" in
  let re_decimal_float = Str.regexp "^\\([0-9]+\\.[0-9]*\\|[0-9]*\\.[0-9]+\\)\\([eE][-+]?[0-9]+\\)?$" in
  let re_decimal_exp = Str.regexp "^[0-9]+[eE][-+]?[0-9]+$" in
  let re_hex = Str.regexp "^0[xX][0-9a-fA-F]+$" in
  let re_hex_float = Str.regexp "^0[xX][0-9a-fA-F]+\\.[0-9a-fA-F]*\\([pP][-+]?[0-9]+\\)?$\\|^0[xX][0-9a-fA-F]+[pP][-+]?[0-9]+$\\|^0[xX]\\.[0-9a-fA-F]+[pP][-+]?[0-9]+$" in
  Str.string_match re_decimal_int s 0
  || Str.string_match re_decimal_float s 0
  || Str.string_match re_decimal_exp s 0
  || Str.string_match re_hex s 0
  || Str.string_match re_hex_float s 0