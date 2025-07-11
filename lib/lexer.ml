(* lexer.ml *)

type name = string;;
type numeral = float;;
type literalstring = string;;

type token = 
  | Whitespace of string (* Spaces, newlines, tabs, and carriage returns *)
  | Comment of string (* Either multi-line or single-line comments *)
  | StringStart of string | StringEnd of string (* starts and ends of a string. There will be no non-string tokens between these two. *)
  | String of string (* Part of a string that isn't an escape *)
  | Escape of string (* A string escape, like \n, only found inside strings *)
  | Keyword of string (* Keywords. Like "while", "end", "do", etc *)
  | Value of string (* Special values. Only true, false, and nil *) 
  | Ident of string (* Identifier. Variables, function names, etc *)
  | Number of string (* Numbers, including both base 10 (and scientific notation) and hexadecimal *)
  | Symbol of string (* Symbols, like brackets, parenthesis, ., etc *)
  | VarArg (* the '...' *)
  | Operator of string (* Operators, like +, -, %, =, ==, >=, <=, ~=, etc *)
  | LabelStart | LabelEnd (* The starts and ends of labels.  Between them there can only be whitespace and an ident tokens.*)
  | UnIdentified of string;; (* Anything that isn't one of the above tokens. Consider them errors. Invalid escapes are also unidentified. *)

type chunk = block
and block = stat list * retstat option 
and stat =
  | Empty
  | Assignment of varlist * explist
  | FunctionCallStat of functioncall
  | LabelStat of label
  | Break
  | Goto of name
  | Do of block
  | While of exp * block
  | Repeat of block * exp
  | If of exp * block * (exp * block) list * block option
  | ForNum of name * exp * exp * exp option * block
  | ForIn of namelist * explist * block
  | Function of funcname * funcbody
  | LocalFunction of name * funcbody
  | LocalAssign of namelist * explist option
and retstat = RetStat of explist option * bool 
and label = Label of name
and funcname = FuncName of name * name list * name option
and varlist = VarList of var * var list
and var =
  | Name of name
  | Indexed of prefixexp * exp
  | Field of prefixexp * name
and namelist = NameList of name * name list
and explist =  ExpList of exp * exp list
and exp =
  | Nil | False | True | Numeral of numeral | LiteralString of literalstring
  | ExpEllipsis | ExpFunctionDef of functiondef
  | PrefixExp of prefixexp
  | ExpTableConstructor of tableconstructor
  | BinOp of exp * binop * exp
  | UnOp of unop * exp
and prefixexp =
  | Var of var
  | FunctionCall of functioncall
  | Parens of exp
and functioncall =
  | Call of prefixexp * args
  | MethodCall of prefixexp * string * args
and args =
  | ArgsExpList of explist
  | ArgsTable of tableconstructor
  | ArgsString of string
and functiondef = FunctionDef of funcbody
and funcbody = FuncBody of parlist * block
and parlist = 
  | Parlist of namelist * parlist option
  | ParEllipsis
and tableconstructor = TableConstructor of fieldlist
and fieldlist = (field * (fieldsep * field) list * fieldsep option )
and field =
  | FieldExp of exp * exp
  | FieldName of string * exp
  | FieldSimple of exp
and fieldsep =
  | Comma | Semicolon
and binop = Add | Sub | Mul | Div | FloorDiv | Pow | Mod
          | Band | Bxor | Bor | Shr | Shl | Concat
          | Lt | Le | Gt | Ge | Eq | Ne
          | And | Or
and unop = Neg | Not | Len | Bnot;;

let rec drop n l =
  if n <= 0 then l
  else match l with
    | [] -> []
    | _ :: tl -> drop (n - 1) tl

    
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

let rec collect_string _start _end f acc = function
  | (Symbol s)::t when s = (String.make 1 (_end.[0])) -> (StringStart _start)::(String acc)::(StringEnd _end)::(f (drop ((String.length _end)-2) t))
  | (Ident s)::t | (Whitespace s)::t | (Symbol s)::t -> collect_string _start _end f (acc^s) t
  | _ -> failwith "malformed string " 
let rec count_equals count = function
  | (Symbol "[")::t -> (count,t)
  | (Symbol "=")::t -> count_equals (count + 1) t
  | _ -> failwith "malformed string big begin" 

let tokenise (str: string) = 
  (* TODO : comment, string, label_end*)

  let rec coarse_split acc = function
  | [] -> if acc <> "" then [Ident acc] else []
  | c::tl when is_whitespace c ->  let w = Whitespace (String.make 1 c)  in
      if acc <> "" then
        let t = Ident acc in t::w::(coarse_split "" tl)
      else w::(coarse_split "" tl)
  | c::tl when is_symbol c ->  let s = Symbol (String.make 1 c)  in
      if acc <> "" then
        let t = Ident acc in t::s::(coarse_split "" tl)
      else s::(coarse_split "" tl)
  | c::tl -> let current = (acc ^ String.make 1 c) in coarse_split current tl


in let rec refine  = function
  | (Whitespace a)::(Whitespace b)::tl -> refine ((Whitespace (a^b))::tl)

  | (Ident s)::tl when is_keyword s -> (Keyword s)::(refine tl)

  | (Ident s)::tl when is_value s -> (Value s)::(refine tl)

  |(Symbol "'")::tl -> collect_string "'" "'" refine "" tl
  |(Symbol "\"")::tl -> collect_string "\"" "\"" refine "" tl
  |(Symbol "[")::tl -> let (c,t) = count_equals 0 tl in 
      let _start = "["^(String.make c '=')^"[" in 
      let _end = "]"^(String.make c '=')^"]" in 
      collect_string _start _end refine "" t

    
  | (Ident a)::(Symbol ".")::(Ident b)::(Symbol "-")::(Ident c)::tl when is_lua_numeral (a^"."^b^"-"^c) -> (Number (a^"."^b^"-"^c))::(refine tl)
  | (Ident a)::(Symbol ".")::(Ident b)::(Symbol "+")::(Ident c)::tl when is_lua_numeral (a^"."^b^"+"^c) -> (Number (a^"."^b^"+"^c))::(refine tl)
  | (Ident a)::(Symbol ".")::(Ident b)::tl when is_lua_numeral (a^"."^b) -> (Number (a^"."^b))::(refine tl)
  | (Ident a)::(Symbol "-")::(Ident b)::tl when is_lua_numeral (a^"-"^b) -> (Number (a^"-"^b))::(refine tl)
  | (Ident a)::(Symbol "+")::(Ident b)::tl when is_lua_numeral (a^"+"^b) -> (Number (a^"+"^b))::(refine tl)
  | (Ident a)::tl when is_lua_numeral a -> (Number a)::(refine tl)

  | (Symbol ".")::(Symbol ".")::(Symbol ".")::tl -> VarArg::(refine tl)

  | (Symbol ".")::(Symbol ".")::tl -> (Operator "..")::(refine tl)
  | (Symbol "<")::(Symbol "<")::tl -> (Operator "<<")::(refine tl)
  | (Symbol "<")::(Symbol "=")::tl -> (Operator "<=")::(refine tl)
  | (Symbol ">")::(Symbol ">")::tl -> (Operator ">>")::(refine tl)
  | (Symbol ">")::(Symbol "=")::tl -> (Operator ">=")::(refine tl)
  | (Symbol "/")::(Symbol "/")::tl -> (Operator "//")::(refine tl)
  | (Symbol "=")::(Symbol "=")::tl -> (Operator "==")::(refine tl)
  | (Symbol "~")::(Symbol "=")::tl -> (Operator "~=")::(refine tl)
  | (Symbol s)::tl when is_charop s -> (Operator s)::(refine tl)
  | (Ident "or")::tl -> (Operator "or")::(refine tl)
  | (Ident "and")::tl -> (Operator "and")::(refine tl)
  | (Ident "not")::tl -> (Operator "not")::(refine tl)

  | (Symbol ":")::(Symbol ":")::tl -> LabelStart::(refine tl)
  | hd::tl -> hd::(refine tl)
  | [] -> []
in str |> String.to_seq |> List.of_seq |> (coarse_split "") |> refine;;

let parse _ = ([], None );;
let unparse _ = "hello world !!!";;