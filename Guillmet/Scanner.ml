#use "stream.ml"

module Scanner = struct
  type t =
    { token_stream : token Stream.t
    ; char_stream  : char Stream.t
    ; prec_counter : int
    }

  and token =
    | Ident of string * int
    | Boollit of string * int
    | Symlit of string * int
    | Strlit of string * int
    | Charlit of string * int
    | Intlit of string * int
    | Hexlit of string * int
    | Binlit of string * int
    | Octlit of string * int
    | Extlit of string * int
    | Reallit of string * int
    | Ldelim of char * int
    | Rdelim of char * int
    | Meta of char * int

  exception Scan_error
  exception Wrong_symbol
  exception Premature_eof
  exception Mature_eof

  let explode str = List.of_seq (String.to_seq str)
  let implode lst = String.of_seq (List.to_seq lst)


  let empty : t = { token_stream = Stream.of_list [] ; char_stream = Stream.of_list [] ; prec_counter = 0 }

  let of_string str : t = { token_stream = Stream.of_list [] ; char_stream = Stream.of_list (explode str) ; prec_counter = 0 }

  let (++) scn = { scn with prec_counter = scn.prec_counter + 1 }

  let (>>) scn = { scn with prec_counter = scn.prec_counter * 10 }

  let (!!) scn = { scn with prec_counter = 0 }

  let is_whitespace = function
    | '\t' | '\r' | '\n' | ' ' -> true
    | _ -> false

  let is_ldelim = function
    | '(' | '{' | '[' -> true
    | c when c = Char.chr 171 -> true
    | _ -> false

  let is_rdelim = function
    | ')' | '}' | ']' -> true
    | c when c = Char.chr 187 -> true
    | _ -> false

  let is_id_head = function
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '~' | '-'
    | '$' | '%'
    | '^' | '*'
    | '&' | '@'
    | '>' | '<'
    | '=' | '?' -> true
    | _ -> false

  let is_int_head = function
    | '+' | '-' -> true
    | '1' .. '9' -> true
    | _ -> false

  let is_real_head = function
    | '+' | '-' -> true
    | '0' .. '9' -> true
    | _ -> false

  let is_char_head = function
    | '\\' -> true
    | _ -> false

  let is_int_tail = function
    | '0' .. '9' -> true
    | _ -> false

  let is_id_tail c = 
    is_id_head c || is_int_tail c

  let is_real_tail c =
    is_real_head c || c = '.' || c = 'e' || c = 'E' || c = '/'

  let is_char_tail = function
    | ' ' | '\t' | '\r' | '\n' -> false
    | _ -> true

  let is_bool_tail = function
    | '#' | 't' | 'f' -> true
    | _ -> false

  let is_hex_tail = function
    | '#' | 'x' | 'X' -> true
    | 'a' .. 'f' 
    | 'A' .. 'F' -> true
    | '0' .. '9' -> true
    | _ -> false

  let is_oct_lit = function
    | '#' | 'o' | 'O' -> true
    | '0' .. '7' -> true
    | _ -> false

  let is_bin_lit = function
    | '#' | 'b' | 'B' -> true
    | '0' | '1' -> true
    | _ -> false

  let is_ext_tail c = 
    is_real_tail c || c = '#'

  let is_end_token = function
    | Rdelim (_, _) -> true
    | _ -> false

  open Stream

  let rec scan scn =
    match peek_opt scn.char_stream with
    | Some c when is_whitespace c -> Stream.drop_while is_whitespace scn.char_stream; scan scn
    | Some c when is_ldelim c ->
      scn.token_stream <<- Ldelim (next scn.char_stream); scan !!scn
    | Some c when is_rdelim c ->
      scn.token_stream <<- Rdelim (next scn.char_stream); scan >>scn
    | Some c when is_id_head c ->
      scn.token_stream <<- Ident (take_while is_id_tail scn.char_stream |> implode, scn.prec_counter); scan ++scn
    | Some c when is_int_head c ->
      scn.token_stream <<- Intlit (take_while is_int_tail scn.char_stream |> implode, scn.prec_counter); scan ++scn
    | Some c when is_real_head c ->
      scn.token_stream <<- Reallit (take_while is_real_tail scn.char_stream |> implode, scn.prec_counter); scan ++scn
    | Some c when is_char_head c ->
      scn.token_stream <<- Charlit (take_while is_char_tail scn.char_stream |> implode, scn.prec_counter); scan ++scn
    | Some c when c = '#' ->
      Stream.skip_next scn.char_stream;
      match peek_opt scn.char_stream with
      | Some c when c = 't' || c = 'f' ->
        scn.token_stream <<- Boollit (take_while is_bool_tail scn.char_stream |> implode, scn.prec_counter); scan ++scn
      | Some c when c = 'x' || c = 'X' ->
        scn.token_stream <<- Hexlit (take_while is_hex_tail scn.char_stream |> implode, scn.prec_counter); scan ++scn
      | Some c when c = 'O' || c = 'o' ->
        scn.token_stream <<- Octlit (take_while is_oct_tail scn.char_stream |> implode, scn.prec_counter); scan ++scn
      | Some c when c = 'B' || c = 'b' ->
        scn.token_stream <<- Binlit (take_while is_bin_tail scn.char_stream |> implode, scn.prec_counter); scan ++scn
      | Some c when c = 'E' || c = 'e' ->
        scn.token_stream <<- ExtLit (take_while is_ext_tail scn.char_stream |> implode, scn.prec_counter); scan ++scn
      | Some _ -> raise Wrong_symbol
      | None -> raise Premature_eof
    | Some _ -> raise Wrong_symbol
    | None -> 
        if Stream.peek_last scn.token_stream |> is_end_token then raise Mature_eof
        else raise Premature_eof


end
