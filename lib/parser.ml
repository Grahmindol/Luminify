(* parser.ml *)
open Tokeniser

type name = string
type numeral = float
type literalstring = string

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
and unop = Neg | Not | Len | Bnot

let rec split_at_first t = function
| [] -> ([],[])
| hd::tl when hd = t -> ([],tl)
| hd::tl -> let f,s = split_at_first t tl
in (hd::f,s);;

let parse_unop = function
| (Operator "-")::tl -> Some (Neg,tl)
| (Operator "not")::tl -> Some (Not,tl) 
| (Operator "#")::tl -> Some (Len,tl) 
| (Operator "~")::tl -> Some (Bnot,tl)
| _ -> None;;

let parse_binop = function
| (Operator "+")::tl -> Some (Add,tl)
| (Operator "-")::tl -> Some (Sub,tl) 
| (Operator "*")::tl -> Some (Mul,tl) 
| (Operator "/")::tl -> Some (Div,tl)
| (Operator "//")::tl -> Some (FloorDiv,tl)
| (Operator "^")::tl -> Some (Pow,tl) 
| (Operator "%")::tl -> Some (Mod,tl) 
| (Operator "&")::tl -> Some (Band,tl)
| (Operator "~")::tl -> Some (Bxor,tl)
| (Operator "|")::tl -> Some (Bor,tl) 
| (Operator ">>")::tl -> Some (Shr,tl) 
| (Operator "<<")::tl -> Some (Shl,tl)
| (Operator "..")::tl -> Some (Concat,tl)
| (Operator "<")::tl -> Some (Lt,tl) 
| (Operator "<=")::tl -> Some (Le,tl) 
| (Operator ">")::tl -> Some (Gt,tl)
| (Operator ">=")::tl -> Some (Ge,tl)
| (Operator "==")::tl -> Some (Eq,tl) 
| (Operator "~=")::tl -> Some (Ne,tl) 
| (Operator "and")::tl -> Some (And,tl)
| (Operator "or")::tl -> Some (Or,tl)
| _ -> None;;

let parse_fieldsep = function
| (Symbol ";")::tl -> Some (Semicolon,tl)
| (Symbol ",")::tl -> Some (Comma,tl)
| _ -> None;;

let rec parse_field = function (*TO DO*)
| (Symbol "[")::tl -> begin
  match split_at_first (Symbol "]") tl with
  | key_tl, _::val_tl -> begin match (parse_exp key_tl, parse_exp val_tl) with
  | Some (key,[]), Some (value, rest) -> Some (FieldExp (key,value), rest)
  | _ -> None end
  | _ -> None end
| (Ident s)::(Symbol "=")::tl -> 
  begin match parse_exp tl with
  | Some (value, rest) -> Some (FieldName (s,value),rest)
  | _ -> None
  end
| _ as tl -> 
  match parse_exp tl with
  | Some (value, rest) -> Some (FieldSimple value,rest)
  | _ -> None

and parse_fieldlist l =
  match parse_field l with
  | None -> None
  | Some (field, rest) ->
    match parse_fieldsep rest with
  | None -> Some ((field, [], None), rest)
  | Some (sep, rest_out) ->
    match parse_fieldlist rest_out with
  | None -> Some ((field, [], Some sep), rest_out)
  | Some ((next, nl, final_sep), rest_out) ->
    Some ((field, (sep, next) :: nl, final_sep), rest_out)


and parse_tableconstructor = function (*TO DO*)
| (Symbol "{")::_ -> None
| _ -> None

and parse_parlist = function (*TO DO*)
| VarArg::tl -> Some (ParEllipsis,tl)
| _ -> None

and parse_funcbody = function (*TO DO*)
| (Symbol "(")::_ -> None
| _ -> None

and parse_functiondef = function (*TO SEE*)
| (Keyword "function")::tl -> begin 
  match parse_funcbody tl with
  | None -> None
  | Some (b,r) -> Some (FunctionDef b,r)
  end
| _ -> None

and parse_args =  function (*TO DO*)
| (Symbol "(")::_ -> None
| (StringStart _)::(String s)::(StringEnd _)::tl -> Some (ArgsString s,tl)
| _ -> None

and parse_functioncall _ = None

and parse_prefixexp = function (*TO DO*)
| (Symbol "(")::_ -> None
| _ -> None

and parse_exp = function
| (Value "nil")::tl -> Some (Nil,tl)
| (Value "false")::tl -> Some (False,tl)
| (Value "true")::tl -> Some (True,tl) 
| (Number _)::tl -> Some (Numeral 0.0,tl) (*TO DO*)
| (StringStart _)::(String s)::(StringEnd _)::tl -> Some (LiteralString s, tl)
| VarArg::tl -> Some (ExpEllipsis,tl)
| _ -> None(*TO DO*)

and parse_explist l = match parse_exp l with
| None -> None
| Some (exp, (Symbol ",")::rest) -> begin
  match parse_explist rest with
  | None -> None
  | Some (ExpList (next,nl), rest_out) -> Some (ExpList (exp,next::nl), rest_out)
end
| Some (exp, rest) -> Some (ExpList (exp,[]), rest)

and parse_namelist = function
| (Ident s)::(Symbol ",")::tl -> begin
  match parse_namelist tl with
  | None -> None
  | Some (NameList (next,nl), rest_out) -> Some (NameList (s,next::nl), rest_out)
end
| (Ident s)::tl -> Some (NameList (s,[]), tl)
| _ -> None

and parse_var = function (*TO DO*)
| (Ident s)::tl -> Some (Name s,tl)
| _ -> None

and parse_varlist l = match parse_var l with
| None -> None
| Some (var, (Symbol ",")::rest) -> begin
  match parse_varlist rest with
  | None -> None
  | Some (VarList (next,nl), rest_out) -> Some (VarList (var,next::nl), rest_out)
end
| Some (var, rest) -> Some (VarList (var,[]), rest)

and parse_funcname = function
| (Ident s)::(Symbol ".")::tl -> begin
  match parse_funcname tl with
  | None -> None
  | Some (FuncName (next,nl,met), rest_out) -> Some (FuncName (s,next::nl, met), rest_out)
end
| (Ident s)::(Symbol ":")::(Ident met)::tl -> Some (FuncName (s,[],Some met), tl)
| (Ident s)::tl -> Some (FuncName (s,[],None), tl)
| _ -> None

and parse_label = function
| LabelStart::(Ident s)::LabelEnd::tl -> Some (Label s, tl) 
| _ -> None

and parse_retstat = function (*TO DO*)
| (Keyword "return")::(Symbol ";")::rest -> Some (RetStat (None,true), rest)
| (Keyword "return")::_ -> None
| _ -> None

and parse_stat = function
| (Symbol ";")::tl -> Some (Empty,tl)
| (Keyword "break")::tl -> Some (Break,tl)
| (Keyword "goto")::(Ident name)::tl -> Some (Goto name,tl)
| (Keyword "do")::_ ->  None;
| (Keyword "while")::_ -> None;
| (Keyword "repeat")::_ -> None;
| (Keyword "if")::_ -> None;
| (Keyword "for")::_ -> None;
| (Keyword "function")::_ -> None;
| (Keyword "local")::(Keyword "function")::_ -> None;
| (Keyword "local")::_ -> None;
| _ -> None

and parse_block l = match parse_retstat l with
| None -> begin 
  match parse_stat l with 
  | None -> (([], None),l)
  | Some (stat, rest) -> let ((prev,ret),rest_out) =  parse_block rest in
    ((stat::prev, ret),rest_out)
  end
| Some (ret, rest) -> (([], Some ret),rest)

and parse_chunk t =
  let rec rem_white_space = function
  | (Whitespace _)::tl| (Comment _)::tl -> rem_white_space tl
  | hd::tl -> hd::(rem_white_space tl)
  | [] -> []
  in t |> rem_white_space |> parse_block;;
  

let parse intput = 
  let tokens = Tokeniser.filter_useless_tokens (Tokeniser.tokenise intput) in
  ignore tokens;
  print_endline (Tokeniser.untokenise tokens);
  ([], None );;
let unparse _ = "hello world !!!";;