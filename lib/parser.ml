(* parser.ml *)
open Tokeniser
open Utils

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
  | Nil | False | True | Numeral of string | LiteralString of string
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
| (Symbol "[")::tl -> 
  begin match parse_exp tl with
  | Some (key,(Symbol "]")::(Symbol "=")::tl_sec) -> 
    begin match parse_exp tl_sec with
    | Some (value,rest) -> Some (FieldExp (key,value), rest)
    | _ -> failwith "value exepted after '='" 
    end
  | _ -> failwith "key exepted after '['"  
  end
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
| (Symbol "{")::tl -> 
  begin match parse_fieldlist tl with
  | Some (fl, (Symbol "}")::rest) -> Some (TableConstructor (Some fl), rest)
  | _ -> failwith "only field exepted between '{' and '}'"
  end 
| _ -> None

and parse_parlist = function
| VarArg::tl -> Some (ParEllipsis,tl)
| l -> match parse_namelist l with 
| Some (nl, (Symbol ",")::VarArg::rest) -> Some (Parlist (nl, true), rest) 
| Some (nl, rest) -> Some (Parlist (nl, false), rest)
| None -> None

and parse_funcbody = function
| (Symbol "(")::(Symbol ")")::block_tl -> 
  begin match parse_block block_tl with 
  | bl , (Keyword "end")::rest_out ->  Some (FuncBody (None, bl), rest_out)
  | _ -> failwith "'end' exepted after block"
  end
| (Symbol "(")::tl -> 
  begin match parse_parlist tl with
  | Some (p, (Symbol ")")::block_tl) ->
    begin match parse_block block_tl with 
    | bl , (Keyword "end")::rest_out ->  Some (FuncBody (Some p, bl), rest_out)
    | _ -> failwith "'end' exepted after block"
    end
  | _ -> failwith "')' exepted after parameter list"
  end
| _ -> None

and parse_functiondef = function
| (Keyword "function")::tl -> begin 
  match parse_funcbody tl with
  | None -> None
  | Some (b,r) -> Some (FunctionDef b,r)
  end
| _ -> None

and parse_args =  function
| (Symbol "(")::(Symbol ")")::rest -> Some (ArgsExpList None, rest)
| (Symbol "(")::tl -> 
  begin match parse_explist tl with
  | Some (el, (Symbol ")")::rest) -> Some (ArgsExpList (Some el), rest)
  | _ -> failwith "')' exepted after explist "
  end
| (StringStart _)::(String s)::(StringEnd _)::tl -> Some (ArgsString s,tl)
| l -> match parse_tableconstructor l with 
| Some (tc, rest) -> Some (ArgsTable tc, rest)
| None -> None

and parse_functioncall l = 
  match parse_prefixexp l with 
  | Some (FunctionCall fc, rest) -> Some (fc,rest)
  | _ -> None

and parse_prefixexp = function
| (Symbol "(")::tl -> 
  begin match parse_exp tl with
  | Some (exp, (Symbol ")")::rest) -> Some (Parens exp, rest)
  | _ -> failwith "')' expected after '(' ..."
  end
| (Ident s)::tl ->
  let rec aux prev = function
    | (Symbol "[")::exp_tl -> 
        begin match parse_exp exp_tl with 
        | Some (exp, (Symbol "]")::rest) -> aux (Var (Indexed (prev, exp))) rest
        | _ -> failwith "']' expected after '['"
        end
    | (Symbol ".")::name_tl ->
        begin match name_tl with 
        | (Ident n)::rest -> aux (Var (Field (prev, n))) rest
        | _ -> failwith "name expected after '.'"
        end
    | (Symbol ":")::name_tl -> 
        begin match name_tl with
        | (Ident name)::args_tl ->
            begin match parse_args args_tl with
            | Some (ar, rest_out) -> aux (FunctionCall (MethodCall (prev, name, ar))) rest_out
            | None -> failwith "args expected after method name"
            end
        | _ -> failwith "method name expected after ':'"
        end
    | rest ->
        begin match parse_args rest with
        | Some (ar, rest_out) -> aux (FunctionCall (Call (prev, ar))) rest_out
        | None -> (prev, rest)
        end
    in let final_exp, rest = aux (Var (Name s)) tl in
  Some (final_exp, rest)
| _ -> None

and parse_exp l =
  let rec insert_binop left_tree op right_tree =
    let precedence_of_binop = function
      | Or        -> 1
      | And       -> 2
      | Lt | Gt | Le | Ge | Ne | Eq -> 3
      | Bor       -> 4
      | Bxor      -> 5
      | Band      -> 6
      | Shl | Shr -> 7
      | Concat    -> 8
      | Add | Sub -> 9
      | Mul | Div | FloorDiv | Mod -> 10
      | Pow       -> 12
  in match left_tree with
      | BinOp (l1, op1, r1) ->
        let prec1 = precedence_of_binop op1 in
        let prec2 = precedence_of_binop op in
        let assoc2 = match op with 
          | Concat | Pow -> true 
          | _ -> false in

        if prec1 < prec2 || (prec1 = prec2 && assoc2) then
          (* le nouveau op a plus de priorité, on descend à droite *)
          BinOp (l1, op1, insert_binop r1 op right_tree)
        else
      (* le nouveau op prend le dessus *)
          BinOp (left_tree, op, right_tree)
      | _ -> BinOp (left_tree, op, right_tree)
  in let rec aux left_exp left_rest =
    match parse_binop left_rest with
    | Some (bi, rest_after_op) ->
      begin match parse_exp rest_after_op with
      | Some (right_exp, rest_out) ->
        aux (insert_binop left_exp bi right_exp) rest_out
      | None -> failwith "expression expected after binary operator"
      end
    | None -> Some (left_exp, left_rest)
  in match l with
  | (Value "nil") :: tl -> aux Nil tl
  | (Value "false") :: tl -> aux False tl
  | (Value "true") :: tl -> aux True tl
  | (Number n) :: tl -> aux (Numeral n) tl
  | (StringStart _) :: (String s) :: (StringEnd _) :: tl -> aux (LiteralString s) tl
  | VarArg :: tl -> aux ExpEllipsis tl
  | _ -> match parse_functiondef l with
    | Some (fd, rest) -> aux (ExpFunctionDef fd) rest
  | None -> match parse_prefixexp l with
    | Some (pe, rest) -> aux (PrefixExp pe) rest
  | None -> match parse_tableconstructor l with
    | Some (tc, rest) -> aux (ExpTableConstructor tc) rest
  | None ->  match parse_unop l with
    | Some (un, rest_after_unop) ->(match parse_exp rest_after_unop with
      | Some (BinOp (a, Pow, b), rest_out) -> aux (BinOp (UnOp (un, a), Pow, b)) rest_out
      | Some (e, rest_out) -> aux (UnOp (un, e)) rest_out
      | None -> failwith "expression expected after unary operator")
  | None -> None

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

and parse_var l = match parse_prefixexp l with 
| Some (Var v, rest) -> Some (v, rest)
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

(*--------------- unparser ---------------*)

let is_name s =
  let len = String.length s in
     len <> 0 
  && (s.[0] < '0' || s.[0] > '9') 
  && not (is_keyword s || is_value s)
  && let rec check i = i >= len || (is_latin_or_underscore s.[i] && check (i + 1))
     in check 0

let unparse_unop = function
| Neg -> [Operator "-"]
| Not -> [Operator "not"]
| Len -> [Operator "#"]
| Bnot -> [Operator "~"]

let unparse_binop = function
| Add -> [Operator "+"]
| Sub -> [Operator "-"]
| Mul -> [Operator "*"]
| Div -> [Operator "/"]
| FloorDiv -> [Operator "//"]
| Pow -> [Operator "^"]
| Mod -> [Operator "%"]
| Band -> [Operator "&"]
| Bxor -> [Operator "~"]
| Bor -> [Operator "|"]
| Shr -> [Operator ">>"]
| Shl -> [Operator "<<"]
| Concat -> [Operator ".."]
| Lt -> [Operator "<"]
| Le -> [Operator "<="]
| Gt -> [Operator ">"]
| Ge -> [Operator ">="]
| Eq -> [Operator "=="]
| Ne -> [Operator "~="]
| And -> [Operator "and"]
| Or -> [Operator "or"]

let rec unparse_field = function
| FieldExp (k,v) -> (Symbol "[")::(unparse_exp k)@(Symbol "]")::(Symbol "=")::(unparse_exp v)
| FieldName (n,v) -> (Ident n)::(Symbol "=")::(unparse_exp v)
| FieldSimple e -> unparse_exp e

and unparse_tableconstructor = function
| TableConstructor Some (f,sfl, _) -> 
  (Symbol "{")
  ::(unparse_field f)
  @(List.concat (List.map (fun (_,f) -> (Symbol ",")::(unparse_field f)) sfl))
  @ [(Symbol "}")]
| TableConstructor None -> [(Symbol "{");(Symbol "}")]

and unparse_parlist = function
| Parlist (nl, false) -> unparse_namelist nl
| Parlist (nl, true) -> (unparse_namelist nl)@[(Symbol ",");VarArg]
| ParEllipsis -> [VarArg]

and unparse_funcbody = function
| FuncBody (None, bl) -> (Symbol "(")::(Symbol ")")::(unparse_block bl)@[Keyword "end"]
| FuncBody (Some pa, bl) -> (Symbol "(")::(unparse_parlist pa)@(Symbol ")")::(unparse_block bl)@[Keyword "end"]

and unparse_functiondef = function
| FunctionDef fb -> (Keyword "function")::(unparse_funcbody fb)

and unparse_args = function
| ArgsExpList None -> [(Symbol "(");(Symbol ")")]
| ArgsExpList (Some (ExpList (LiteralString s,[]))) -> unparse_exp (LiteralString s)
| ArgsExpList (Some (ExpList (ExpTableConstructor tc,[]))) -> unparse_tableconstructor tc
| ArgsExpList (Some el) -> (Symbol "(")::(unparse_explist el)@[(Symbol ")")]
| ArgsTable tc -> unparse_tableconstructor tc
| ArgsString s -> unparse_exp (LiteralString s)

and unparse_functioncall = function
| Call (pe,ar) -> (unparse_prefixexp pe)@(unparse_args ar)
| MethodCall (pe, na, ar) -> (unparse_prefixexp pe)@[(Symbol ":");(Ident na)]@(unparse_args ar)

and unparse_prefixexp = function
| Var v -> unparse_var v
| FunctionCall fc -> unparse_functioncall fc
| Parens ex ->  (Symbol "(")::(unparse_exp ex)@[(Symbol ")")]

and unparse_exp = function
| Nil -> [(Value "nil")]
| False -> [(Value "false")]
| True -> [(Value "true")]
| Numeral n -> [(Number n)]
| LiteralString s -> 
  if not (String.contains s '\'') then
    [StringStart "'"; String s; StringEnd "'"]
  else if not (String.contains s '"') then
    [StringStart "\""; String s; StringEnd "\""]
  else 
      let string_contains_sub s sub =
        let len_s = String.length s in
        let len_sub = String.length sub in
        let rec aux i =
          if i + len_sub > len_s then false
          else if String.sub s i len_sub = sub then true
          else aux (i + 1)
        in aux 0
      in let find_smallest_closing s =
        let rec aux n =
        let opening = "[" ^ String.make n '=' ^ "[" in
        let closing = "]" ^ String.make n '=' ^ "]" in
        if string_contains_sub s closing then
          aux (n + 1)
        else
          (opening,closing)
        in aux 0
      in let o,c = find_smallest_closing s
    in [StringStart o; String s; StringEnd c]
| ExpEllipsis -> [VarArg]
| ExpFunctionDef fd -> unparse_functiondef fd
| PrefixExp pe -> unparse_prefixexp pe
| ExpTableConstructor tc -> unparse_tableconstructor tc
| BinOp (a, bi, b) -> (unparse_exp a)@(unparse_binop bi)@(unparse_exp b)
| UnOp (un,a) -> (unparse_unop un)@(unparse_exp a)

and unparse_explist = function
| ExpList (e, []) -> unparse_exp e
| ExpList (e, hd::tl) -> (unparse_exp e)@(Symbol ",")::(unparse_explist (ExpList (hd,tl)))

and unparse_namelist = function
| NameList (n, []) -> [Ident n]
| NameList (n, hd::tl) -> (Ident n)::(Symbol ",")::(unparse_namelist (NameList (hd,tl)))

and unparse_var = function
| Name n -> [Ident n]
| Indexed (pe, LiteralString s) when is_name s -> (unparse_prefixexp pe)@[(Symbol ".");(Ident s)]
| Indexed (pe,ex) -> (unparse_prefixexp pe)@(Symbol "[")::(unparse_exp ex)@[(Symbol "]")]
| Field (pe,n) -> (unparse_prefixexp pe)@[(Symbol ".");(Ident n)]

and unparse_varlist = function
| VarList (v, []) -> unparse_var v
| VarList (v, hd::tl) -> (unparse_var v)@(Symbol ",")::(unparse_varlist (VarList (hd,tl)))

and unparse_funcname = function
| FuncName (n, [], None) -> [Ident n]
| FuncName (n, [], Some m) -> [(Ident n);(Symbol ":");(Ident m)]
| FuncName (n, hd::tl, m) -> (Ident n)::(Symbol ".")::(unparse_funcname (FuncName (hd, tl, m)))

and unparse_label = function
| Label l -> [LabelStart;(Ident l);LabelEnd]

and unparse_retstat = function
| RetStat (None, _) -> [Keyword "return"]
| RetStat (Some el, _) -> (Keyword "return")::(unparse_explist el) 

and unparse_stat = function
| Empty -> []
| Assignment (vl,el) -> (unparse_varlist vl)@(Symbol "=")::(unparse_explist el)
| FunctionCallStat fc -> unparse_functioncall fc
| LabelStat l -> unparse_label l
| Break -> [Keyword "break"]
| Goto n -> [Keyword "goto"; Ident n]
| Do bl -> (Keyword "do")::(unparse_block bl)@[(Keyword "end")]
| While (exp,bl) -> (Keyword "while")::(unparse_exp exp)@(Keyword "do")::(unparse_block bl)@[(Keyword "end")]
| Repeat (bl, exp) -> (Keyword "repeat")::(unparse_block bl)@(Keyword "until")::(unparse_exp exp)
| If (e,b, ebl, el) -> (Keyword "if")::(unparse_exp e)@(Keyword "then")::(unparse_block b)@
  (let rec aux = function
  | (e,b)::tl -> (Keyword "elseif")::(unparse_exp e)@(Keyword "then")::(unparse_block b)@(aux tl)
  | [] -> match el with
  | Some bl -> (Keyword "else")::(unparse_block bl)@[(Keyword "end")]
  | None -> [Keyword "end"]
  in aux ebl)
| ForNum (n,a,b,c,bl) -> (Keyword "for")::(Ident n)::(Symbol "=")::(unparse_exp a)@(Symbol ",")::(unparse_exp b)@
  (match c with Some e -> (Symbol ",")::(unparse_exp e) | _ -> [])@(Keyword "do")::(unparse_block bl)@[(Keyword "end")]
| ForIn (nl, el, bl) -> (Keyword "for")::(unparse_namelist nl)@(Keyword "in")::(unparse_explist el)@(Keyword "do")::(unparse_block bl)@[(Keyword "end")]
| Function (fn,fb) -> (Keyword "function")::(unparse_funcname fn)@(unparse_funcbody fb)
| LocalFunction (n,fb) -> (Keyword "local")::(Keyword "function")::(Ident n)::(unparse_funcbody fb)
| LocalAssign (nl, Some el) -> (Keyword "local")::(unparse_namelist nl)@(Keyword "=")::(unparse_explist el)
| LocalAssign (nl, None) -> (Keyword "local")::(unparse_namelist nl)

and unparse_block = function
| [], None -> []
| [], Some r -> unparse_retstat r
| hd::tl, r -> (unparse_stat hd)@(unparse_block (tl,r))

and unparse_chunk c = 
  let rec add_important_space = function
  | (StringStart a)::(String b)::(StringEnd c)::tl -> (StringStart a)::(String b)::(StringEnd c)::(add_important_space tl)
  | a::b::tl when 
      (let s = token_to_string a in is_latin_or_underscore (s.[String.length s - 1]))
    &&(let s = token_to_string b in is_latin_or_underscore (s.[0]))
  -> a::(Whitespace " ")::(add_important_space (b::tl))
  | hd::tl -> hd::(add_important_space tl)
  | [] -> []
in c |> unparse_block |> add_important_space
  

let parse i = let c,_ = parse_chunk (tokenise i) in c;;
let unparse c = untokenise (unparse_chunk c);;