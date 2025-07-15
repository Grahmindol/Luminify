open Tokeniser
type name = string
type numeral = float
type literalstring = string

type chunk = block
and block = stat list * retstat option
and stat =
    Empty
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
    Name of name
  | Indexed of prefixexp * exp
  | Field of prefixexp * name
and namelist = NameList of name * name list
and explist = ExpList of exp * exp list
and exp =
    Nil
  | False
  | True
  | Numeral of string
  | LiteralString of string
  | ExpEllipsis
  | ExpFunctionDef of functiondef
  | PrefixExp of prefixexp
  | ExpTableConstructor of tableconstructor
  | BinOp of exp * binop * exp
  | UnOp of unop * exp
and prefixexp = Var of var | FunctionCall of functioncall | Parens of exp
and functioncall =
    Call of prefixexp * args
  | MethodCall of prefixexp * string * args
and args =
    ArgsExpList of explist option
  | ArgsTable of tableconstructor
  | ArgsString of string
and functiondef = FunctionDef of funcbody
and funcbody = FuncBody of parlist option * block
and parlist = Parlist of namelist * bool | ParEllipsis
and tableconstructor = TableConstructor of fieldlist option
and fieldlist = field * (fieldsep * field) list * fieldsep option
and field =
    FieldExp of exp * exp
  | FieldName of string * exp
  | FieldSimple of exp
and fieldsep = Comma | Semicolon
and binop =
    Add
  | Sub
  | Mul
  | Div
  | FloorDiv
  | Pow
  | Mod
  | Band
  | Bxor
  | Bor
  | Shr
  | Shl
  | Concat
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Ne
  | And
  | Or
and unop = Neg | Not | Len | Bnot

val parse_unop : token list -> (unop * token list) option
val parse_binop : token list -> (binop * token list) option 
val parse_fieldsep : token list -> (fieldsep * token list) option 
val parse_field : token list -> (field * token list) option 
val parse_fieldlist : token list -> (fieldlist * token list) option
val parse_tableconstructor : token list -> (tableconstructor * token list) option
val parse_parlist : token list -> (parlist * token list) option 
val parse_funcbody : token list -> (funcbody * token list) option 
val parse_functiondef : token list -> (functiondef * token list) option 
val parse_args : token list -> (args * token list) option 
val parse_functioncall : token list -> (functioncall * token list) option 
val parse_prefixexp : token list -> (prefixexp * token list) option 
val parse_exp : token list -> (exp * token list) option 
val parse_explist : token list -> (explist * token list) option 
val parse_namelist : token list -> (namelist * token list) option 
val parse_var : token list -> (var * token list) option 
val parse_varlist : token list -> (varlist * token list) option 
val parse_funcname : token list -> (funcname * token list) option 
val parse_label : token list -> (label * token list) option 
val parse_retstat : token list -> (retstat * token list) option 
val parse_stat : token list -> (stat * token list) option 
val parse_block : token list -> block * token list
val parse_chunk : token list -> chunk * token list 

val parse : string -> chunk
val unparse : chunk -> string