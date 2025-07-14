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
  | ArgsExpList of explist option
  | ArgsTable of tableconstructor
  | ArgsString of string
and functiondef = FunctionDef of funcbody
and funcbody = FuncBody of parlist option * block
and parlist = 
  | Parlist of namelist * bool
  | ParEllipsis
and tableconstructor = TableConstructor of fieldlist option
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

let rec parse_field = function
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


and parse_tableconstructor = function
| (Symbol "{")::(Symbol "}")::rest -> Some (TableConstructor None, rest)
| (Symbol "{")::tl -> let field_list_tl, rest = split_at_first (Symbol "}") tl
  in begin match parse_fieldlist  field_list_tl with 
  | Some (fl,[]) -> Some (TableConstructor (Some fl), rest)
  | _ -> None end
| _ -> None

and parse_parlist = function
| VarArg::tl -> Some (ParEllipsis,tl)
| l -> match parse_namelist l with 
| Some (nl, (Symbol ",")::VarArg::rest) -> Some (Parlist (nl, true), rest) 
| Some (nl, rest) -> Some (Parlist (nl, false), rest)
| None -> None

and parse_funcbody = function
| (Symbol "(")::tl -> let parlist_tl, rest = split_at_first (Symbol ")") tl
in let block_tl, rest_out = split_at_first (Keyword "end") rest
in let par = match parse_parlist parlist_tl with
  | Some (p,[]) -> Some p
  | _ -> None
in let bl,_ = parse_block block_tl
in Some (FuncBody (par, bl), rest_out)
| _ -> None

and parse_functiondef = function
| (Keyword "function")::tl -> begin 
  match parse_funcbody tl with
  | None -> None
  | Some (b,r) -> Some (FunctionDef b,r)
  end
| _ -> None

and parse_args =  function
| (Symbol "(")::tl -> let explist_tl, rest = split_at_first (Symbol ")") tl
in let exp = match parse_explist explist_tl with 
  | Some (exp,[]) -> Some exp
  | _ -> None
in Some (ArgsExpList exp, rest)
| (StringStart _)::(String s)::(StringEnd _)::tl -> Some (ArgsString s,tl)
| l -> match parse_tableconstructor l with 
| Some (tc, rest) -> Some (ArgsTable tc, rest)
| None -> None

and parse_functioncall l = match parse_prefixexp l with 
| None -> None
| Some (pe, (Symbol ":")::(Ident name)::rest) -> begin match parse_args rest with
  | None -> None
  | Some (ar,rest_out) -> Some (MethodCall (pe,name,ar), rest_out) end
| Some (pe, rest) -> begin match parse_args rest with
  | None -> None
  | Some (ar,rest_out) -> Some (Call (pe,ar), rest_out) end

and parse_prefixexp = function
| (Symbol "(")::tl -> let exp_tl, rest = split_at_first (Symbol ")") tl
in begin match parse_exp exp_tl with
  | Some (exp,[]) -> Some (Parens exp,rest)
  | _ -> None
end
| l -> match parse_var l with
| Some (var, rest) -> Some (Var var,rest) 
| None -> match parse_functioncall l with
| Some (fc, rest) -> Some (FunctionCall fc, rest)
| None -> None

and parse_exp l =
  match (
    match l with
    | (Value "nil") :: tl -> Some (Nil, tl)
    | (Value "false") :: tl -> Some (False, tl)
    | (Value "true") :: tl -> Some (True, tl)
    | (Number _) :: tl -> Some (Numeral 0.0 (* TODO: parse number *), tl)
    | (StringStart _) :: (String s) :: (StringEnd _) :: tl -> Some (LiteralString s, tl)
    | VarArg :: tl -> Some (ExpEllipsis, tl)
    | l -> 
      (* functiondef *)
      (match parse_functiondef l with
      | Some (fd, rest) -> Some (ExpFunctionDef fd, rest)
      | None ->
        (* prefixexp *)
        match parse_prefixexp l with
        | Some (pe, rest) -> Some (PrefixExp pe, rest)
        | None ->
          (* tableconstructor *)
          match parse_tableconstructor l with
          | Some (tc, rest) -> Some (ExpTableConstructor tc, rest)
          | None ->
            (* unop exp *)
            match parse_unop l with
            | Some (un, rest) -> 
              (match parse_exp rest with
              | Some (e, rest_out) -> Some (UnOp (un, e), rest_out)
              | None -> None)
            | None -> None))
  with
  | None -> None
  | Some (exp, rest) ->
    (* binop exp *)
    match parse_binop rest with
    | None -> Some (exp, rest)
    | Some (bi, r) ->
      (match parse_exp r with
      | None -> None
      | Some (sec, rest_out) -> Some (BinOp (exp, bi, sec), rest_out))

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
  | None -> Some (NameList (s,[]), (Symbol ",")::tl)
  | Some (NameList (next,nl), rest_out) -> Some (NameList (s,next::nl), rest_out)
end
| (Ident s)::tl -> Some (NameList (s,[]), tl)
| _ -> None

and parse_var = function
| (Ident s)::tl -> Some (Name s,tl)
| l -> match parse_prefixexp l with
| Some (pe,(Symbol ".")::(Ident name)::rest) -> Some (Field (pe,name), rest)
| Some (pe,(Symbol "[")::rest) -> let exp_tl, rest_out = split_at_first (Symbol "]") rest
  in begin match parse_exp exp_tl with 
  | Some (exp, []) -> Some (Indexed (pe,exp), rest_out)
  | _ -> None 
  end
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

and parse_retstat = function
| (Keyword "return")::tl -> begin match parse_explist tl, tl with
  | Some (expl, (Symbol ";")::rest), _ -> Some (RetStat (Some expl,true), rest)
  | Some (expl, rest), _ -> Some (RetStat (Some expl,false), rest)
  | None, (Symbol ";")::rest -> Some (RetStat (None,true), rest)
  | None, rest -> Some (RetStat (None,false), rest) end
| _ -> None


and parse_stat = function
  | (Symbol ";") :: tl -> Some (Empty, tl)
  | (Keyword "break") :: tl -> Some (Break, tl)
  | (Keyword "goto") :: (Ident name) :: tl -> Some (Goto name, tl)

  | (Keyword "do") :: tl ->
    begin match parse_block tl with
      | bl, (Keyword "end") :: rest_out -> Some (Do bl, rest_out)
      | _ -> failwith "expected 'end' after 'do'"
    end

  | (Keyword "while") :: tl ->
    begin match parse_exp tl with
      | Some (exp, (Keyword "do") :: rest) ->
        begin match parse_block rest with
          | bl, (Keyword "end") :: rest_out -> Some (While (exp, bl), rest_out)
          | _ -> failwith "expected 'end' after 'while ... do'"
        end
      | _ -> failwith "expected 'do' after 'while' expression"
    end

  | (Keyword "repeat") :: tl ->
    begin match parse_block tl with
      | bl, (Keyword "until") :: rest ->
        begin match parse_exp rest with
          | Some (exp, rest_out) -> Some (Repeat (bl, exp), rest_out)
          | _ -> failwith "expected expression after 'until'"
        end
      | _ -> failwith "expected 'until' after 'repeat' block"
    end

  | (Keyword "if") :: tl ->
    begin match parse_exp tl with
      | Some (exp, (Keyword "then") :: rest) ->
        begin match parse_block rest with
          | bl, rest_block ->
            let rec aux = function
              | (Keyword "elseif") :: inner ->
                begin match parse_exp inner with
                  | Some (exp_elif, (Keyword "then") :: block_tl) ->
                    let bl_elif, after = parse_block block_tl in
                    let elifs, else_block, rest_out = aux after in
                    ((exp_elif, bl_elif) :: elifs, else_block, rest_out)
                  | _ -> failwith "expected expression after 'elseif'"
                end
              | (Keyword "else") :: inner ->
                let bl_else, after = parse_block inner in
                begin match after with
                  | (Keyword "end") :: rest_out -> ([], Some bl_else, rest_out)
                  | _ -> failwith "expected 'end' after 'else'"
                end
              | (Keyword "end") :: rest_out -> ([], None, rest_out)
              | _ -> failwith "expected 'end' after 'if' block"
            in
            let elifs, else_block, rest_out = aux rest_block in
            Some (If (exp, bl, elifs, else_block), rest_out)
        end
      | _ -> failwith "expected 'then' after 'if' expression"
    end

  | (Keyword "for") :: (Ident s) :: (Symbol "=") :: tl -> 
    begin match parse_exp tl with
    | Some (a, (Symbol ",")::tl_sec) -> 
      begin match parse_exp tl_sec with 
      | Some (b, (Symbol ",")::tl_thi) ->
        begin match parse_exp tl_thi with
        | Some (c, (Keyword "do")::block_tl) -> 
          begin match parse_block block_tl with
          | bl, (Keyword "end") :: rest_out -> Some (ForNum (s,a,b,Some c,bl), rest_out)
          | _ -> failwith "expected 'end' after 'do'"
          end
        | _ -> failwith "expected a last expression after ',' symbol"
        end
      | Some (b, (Keyword "do")::block_tl) -> 
        begin match parse_block block_tl with
        | bl, (Keyword "end") :: rest_out -> Some (ForNum (s,a,b,None,bl), rest_out)
        | _ -> failwith "expected 'end' after 'do'"
        end
      | _ -> failwith "expected an expression after ',' symbol"
      end
    | _ -> failwith "expected two expression after '=' symbol"
    end

  | (Keyword "for") :: tl -> 
    begin match parse_namelist tl with 
    | Some (nl, (Keyword "in"):: tl_sec) -> 
      begin match parse_explist tl_sec with 
      | Some (el, (Keyword "do"):: block_tl) ->
        begin match parse_block block_tl with
        | bl, (Keyword "end") :: rest_out -> Some (ForIn (nl,el,bl), rest_out)
        | _ -> failwith "expected 'end' after 'do'"
        end
      | _ -> failwith "expected expression list followed by 'do' symbol aftel 'in'"
      end
    | _ -> failwith "expected Name list followed by 'in' symbol aftel 'for'"
    end

  | (Keyword "function") :: tl -> 
    begin match parse_funcname tl with 
    | Some (fn, tl_sec) ->
      begin match parse_funcbody tl_sec with
      | Some (fb , rest_out) -> Some (Function (fn,fb), rest_out)
      | _ -> failwith "expected function body after 'function ...' expression"
      end
    | _ -> failwith "expected a name after 'function' expression"
    end

  | (Keyword "local") :: (Keyword "function") :: (Ident fn) :: tl ->
    begin match parse_funcbody tl with
    | Some (fb , rest_out) -> Some (LocalFunction (fn,fb), rest_out)
    | _ -> failwith "expected function body after 'local function ...' expression"
    end

  | (Keyword "local") :: tl -> 
    begin match parse_namelist tl with
    | Some (nl,  (Symbol "=")::rest) -> 
      begin match parse_explist rest with 
      | Some (el, rest_out) -> Some (LocalAssign (nl, Some el), rest_out)
      | _ -> failwith "expected expression list after 'local ... =' expression"
      end
    | Some (nl, rest_out) -> Some (LocalAssign (nl, None), rest_out)
    | _ -> failwith "expected name list after 'local' expression"
    end 

  | l -> match parse_label l with
    | Some (lab, rest_out) -> Some (LabelStat lab, rest_out)
  | _ -> match parse_functioncall l with
    | Some (fc, rest_out) -> Some (FunctionCallStat fc, rest_out)
  | _ -> match parse_varlist l with
    | Some (vl, (Symbol "=")::rest) -> 
      begin match parse_explist rest with 
      | Some (el, rest_out) -> Some (Assignment (vl,el), rest_out) 
      | _ -> failwith "expected expression list after '... =' expression"
      end
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