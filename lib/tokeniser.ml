(* tokeniser.ml *)
open Utils

type token = 
  | Whitespace of string (* Spaces, newlines, tabs, and carriage returns *)
  | Comment of string (* Either multi-line or single-line comments *)
  | StringStart of string | StringEnd of string (* starts and ends of a string. There will be no non-string tokens between these two. *)
  | String of string (* Part of a string that isn't an escape *)
  | Keyword of string (* Keywords. Like "while", "end", "do", etc *)
  | Value of string (* Special values. Only true, false, and nil *) 
  | Ident of string (* Identifier. Variables, function names, etc *)
  | Number of string (* Numbers, including both base 10 (and scientific notation) and hexadecimal *)
  | Symbol of string (* Symbols, like brackets, parenthesis, ., etc *)
  | VarArg (* the '...' *)
  | Operator of string (* Operators, like +, -, %, =, ==, >=, <=, ~=, etc *)
  | LabelStart | LabelEnd (* The starts and ends of labels.  Between them there can only be whitespace and an ident tokens.*)
  | UnIdentified of string;; (* Anything that isn't one of the above tokens. Consider them errors. Invalid escapes are also unidentified. *)

let token_to_string = function
  | Whitespace s
  | Comment s
  | StringStart s
  | StringEnd s
  | String s
  | Keyword s
  | Value s
  | Ident s
  | Number s
  | Symbol s
  | Operator s
  | UnIdentified s -> s
  | VarArg -> "..."
  | LabelStart
  | LabelEnd -> "::"

let rec count_equals count = function
  | (Symbol "=")::t -> count_equals (count + 1) t
  | _ as t -> (count,t)

let rec collect_string _start _end f acc tokens =
  match tokens with
  | (Symbol s)::t when s = String.make 1 _end.[0] ->
      if String.length _end < 2 then
        (* Simple string end *)
        StringStart _start :: String acc :: StringEnd _end :: f t
      else
        let (c, rest) = count_equals 0 t in
        if c = (String.length _end - 2) then
          match rest with
          | (Symbol "]")::tl -> StringStart _start :: String acc :: StringEnd _end :: f tl
          | _ -> collect_string _start _end f (acc ^ s ^ String.make c '=') rest
        else collect_string _start _end f (acc ^ s ^ String.make c '=') rest
  | (Ident s)::t | (Whitespace s)::t| (Symbol s)::t ->
      collect_string _start _end f (acc ^ s) t
  | _ as t ->
      UnIdentified acc :: f t

let rec collect_comment acc f = function 
  | (Whitespace "\n")::tl -> (Comment (acc^"\n")):: (f tl)
  | t::tl -> collect_comment (acc^(token_to_string t)) f tl
  | [] -> Comment acc :: []

let tokenise (str: string) = 
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

  | (Symbol "-")::(Symbol "-")::(Symbol "[")::(Symbol "[")::tl -> Comment ("--")::(collect_string "[[" "]]" refine "" tl)
  | (Symbol "-")::(Symbol "-")::tl -> collect_comment "--" refine tl

  |(Symbol "'")::tl -> collect_string "'" "'" refine "" tl
  |(Symbol "\"")::tl -> collect_string "\"" "\"" refine "" tl
  |(Symbol "[")::tl -> begin match count_equals 0 tl with 
    | (c,(Symbol "[")::t) -> 
      let _start = "["^(String.make c '=')^"[" in 
      let _end = "]"^(String.make c '=')^"]" in 
      collect_string _start _end refine "" t
    | (0,_) -> (Symbol "[") :: refine tl
    | (c, t) -> (UnIdentified ("["^(String.make c '='))) :: refine t end

    
  | (Ident a)::(Symbol ".")::(Ident b)::(Symbol "-")::(Ident c)::tl when is_lua_numeral (a^"."^b^"-"^c) -> (Number (a^"."^b^"-"^c))::(refine tl)
  | (Ident a)::(Symbol ".")::(Ident b)::(Symbol "+")::(Ident c)::tl when is_lua_numeral (a^"."^b^"+"^c) -> (Number (a^"."^b^"+"^c))::(refine tl)
  | (Ident a)::(Symbol ".")::(Ident b)::tl when is_lua_numeral (a^"."^b) -> (Number (a^"."^b))::(refine tl)
  | (Ident a)::(Symbol "-")::(Ident b)::tl when is_lua_numeral (a^"-"^b) -> (Number (a^"-"^b))::(refine tl)
  | (Ident a)::(Symbol "+")::(Ident b)::tl when is_lua_numeral (a^"+"^b) -> (Number (a^"+"^b))::(refine tl)
  | (Ident a)::tl when is_lua_numeral a -> (Number a)::(refine tl)

  | (Symbol ".")::(Symbol ".")::(Symbol ".")::tl -> VarArg::(refine tl)

  | (Symbol ":")::(Symbol ":")::tl -> LabelStart::(refine tl)

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

  | hd::tl -> hd::(refine tl)
  | [] -> []
in let rec finish  = function
  | (Comment "--")::(StringStart "[[")::(String c)::(StringEnd "]]")::tl -> Comment ("--[["^c^"]]")::(finish tl)

  | LabelStart::(Whitespace a)::(Ident n)::(Whitespace c)::LabelStart::tl -> LabelStart::(Whitespace a)::(Ident n)::(Whitespace c)::LabelEnd::(finish tl)
  | LabelStart::(Whitespace a)::(Ident n)::LabelStart::tl -> LabelStart::(Whitespace a)::(Ident n)::LabelEnd::(finish tl)
  | LabelStart::(Ident n)::(Whitespace c)::LabelStart::tl -> LabelStart::(Ident n)::(Whitespace c)::LabelEnd::(finish tl)
  | LabelStart::(Ident n)::LabelStart::tl -> LabelStart::(Ident n)::LabelEnd::(finish tl)
  | LabelStart::tl -> (UnIdentified "::")::(finish tl)
  
  | hd::tl -> hd::(finish tl)
  | [] -> []
in str |> String.to_seq |> List.of_seq |> (coarse_split "") |> refine |> finish;;

let rec untokenise = function
| hd::tl -> token_to_string hd ^ (untokenise tl)
| [] -> "";;

let rec filter_useless_tokens = function
  | a::(Comment _)::(Comment _)::tl
  | a::(Comment _)::(Whitespace _)::tl
  | a::(Whitespace _)::(Whitespace _)::tl
  | a::(Whitespace _)::(Comment _)::tl ->  filter_useless_tokens (a::(Whitespace " " )::tl)
  | a::(Comment _)::b::tl | a::(Whitespace _)::b::tl when 
      (let s = token_to_string a in is_latin_or_underscore (s.[String.length s - 1]))
    &&(let s = token_to_string b in is_latin_or_underscore (s.[0]))
  -> a::(Whitespace " ")::(filter_useless_tokens (b::tl))
  | (Comment _)::tl | (Whitespace _)::tl -> filter_useless_tokens tl
  | hd::tl -> hd::(filter_useless_tokens tl)
  | [] -> []