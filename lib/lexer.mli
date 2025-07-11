type name = string
type numeral = float
type literalstring = string

type token =
    Whitespace of string
  | Comment of string
  | StringStart
  | StringEnd
  | String of string
  | Escape of string
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
  | Numeral of numeral
  | LiteralString of literalstring
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
    ArgsExpList of explist
  | ArgsTable of tableconstructor
  | ArgsString of string
and functiondef = FunctionDef of funcbody
and funcbody = FuncBody of parlist * block
and parlist = Parlist of namelist * parlist option | ParEllipsis
and tableconstructor = TableConstructor of fieldlist
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

val tokenise : string -> token list
val parse : string -> chunk
val unparse : chunk -> string