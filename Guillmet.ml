module Stream : sig
  type 'a t

  exception Empty_stream
  exception Consume_failed
  exception Peek_failed
  exception Single_skip_failed

  val of_list : 'a list -> 'a t
  val is_spent : 'a t -> bool
  val append : 'a t -> 'a list -> unit
  val (<<-) : 'a t -> 'a -> unit
  val dup : 'a t -> 'a t
  val rev : 'a t -> 'a t

  val peek : 'a t -> 'a
  val next : 'a t -> 'a
  val peek_last : 'a t -> 'a
  val npeek : 'a t -> int -> 'a list
  val peek_opt : 'a t -> 'a option
  val npeek_safe : 'a t -> int -> 'a list option
  val skip_next : 'a t -> 'a t
  val skip : 'a t -> unit

  val remaining : 'a t -> int

  val take_while : ('a -> bool) -> 'a t -> 'a list
  val take_until : ('a -> bool) -> 'a t -> 'a list
  val drop_while : ('a -> bool) -> 'a t -> unit
  val drop_until : ('a -> bool) -> 'a t -> unit
  val skip_single : ('a -> bool) -> 'a t -> unit
end = struct
  type 'a t = 'a Seq.t ref

  exception Empty_stream
  exception Consume_failed
  exception Peek_failed
  exception Single_skip_failed

  let of_list lst = ref (List.to_seq lst)

  let is_spent stm = Seq.is_empty !stm

  let append stm lst = 
    stm := Seq.append !stm (List.to_seq lst)

  let (<<-) stm i = 
    append stm [i]

  let dup stm =
    ref (List.to_seq (List.of_seq !stm))

  let rev stm =
    ref (List.to_seq (List.rev (List.of_seq !stm)))

  let peek stm =
    match !stm () with
    | Seq.Nil -> raise Empty_stream
    | Seq.Cons (hd, _) -> hd

  let next stm =
    match !stm () with
    | Seq.Nil -> raise Empty_stream
    | Seq.Cons (hd, tl) ->
      stm := tl; hd

  let npeek stm n =
    let stm' = dup stm in
    let rec aux acc n' stm'' =
      if n = 0 then List.rev acc
      else aux ((next stm'') :: acc) (n' - 1) stm''
    in
    aux [] n stm'

  let peek_last stm =
    let inv = rev stm in
    peek inv

  let peek_opt stm =
    match !stm () with
    | Seq.Nil -> None
    | Seq.Cons (hd, _) -> Some hd

  let npeek_safe stm n =
    let stm' = dup stm in
    let rec aux acc n' stm'' =
      match peek_opt stm'' with
      | Some _ -> aux ((next stm'') :: acc) (n' - 1) stm''
      | _ -> raise Peek_failed
    in
    aux [] n stm'

  let skip_next stm =
    let _ = next stm in
    stm

  let skip stm =
    let  _ = skip_next stm in
    ()

  let remaining stm = Seq.length !stm

  let take_while pred stm =
    let rec aux acc stm' =
      match peek_opt stm' with
      | Some i when pred i -> aux (i :: acc) (skip_next stm')
      | Some _ -> List.rev acc
      | _ -> raise Consume_failed
    in
    aux [] stm

  let take_until pred stm =
    let rec aux acc stm' =
      match peek_opt stm' with
      | Some i when pred i -> List.rev acc
      | Some i -> aux (i :: acc) (skip_next stm')
      | _ -> raise Consume_failed
    in
    aux [] stm

  let drop_while pred stm =
    let _ = take_while pred stm in
    ()

  let drop_until pred stm =
    let _ = take_until pred stm in
    ()

  let skip_single pred stm =
    match peek_opt stm with
    | Some c when pred c -> next stm; ()
    | Some _ -> raise Single_skip_failed
    | None -> raise Empty_stream
end

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

  let is_oct_tail = function
    | '#' | 'o' | 'O' -> true
    | '0' .. '7' -> true
    | _ -> false

  let is_bin_tail = function
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
      scn.token_stream <<- Ldelim (next scn.char_stream, scn.prec_counter); scan ((!!)scn)
    | Some c when is_rdelim c ->
      scn.token_stream <<- Rdelim (next scn.char_stream, scn.prec_counter); scan ((>>)scn)
    | Some c when is_id_head c ->
      scn.token_stream <<- Ident (take_while is_id_tail scn.char_stream |> implode, scn.prec_counter); scan ((++)scn)
    | Some c when is_int_head c ->
      scn.token_stream <<- Intlit (take_while is_int_tail scn.char_stream |> implode, scn.prec_counter); scan ((++)scn)
    | Some c when is_real_head c ->
      scn.token_stream <<- Reallit (take_while is_real_tail scn.char_stream |> implode, scn.prec_counter); scan ((++)scn)
    | Some c when is_char_head c ->
      scn.token_stream <<- Charlit (take_while is_char_tail scn.char_stream |> implode, scn.prec_counter); scan ((++)scn)
    | None -> 
      if Stream.peek_last scn.token_stream |> is_end_token then raise Mature_eof
      else raise Premature_eof
    | Some c when c = '#' -> 
      let match_pound scn =
        Stream.skip scn.char_stream;
        match peek_opt scn.char_stream with
        | Some c when c = 't' || c = 'f' ->
          scn.token_stream <<- Boollit (take_while is_bool_tail scn.char_stream |> implode, scn.prec_counter); scan ((++)scn)
        | Some c when c = 'x' || c = 'X' ->
          scn.token_stream <<- Hexlit (take_while is_hex_tail scn.char_stream |> implode, scn.prec_counter); scan ((++)scn)
        | Some c when c = 'O' || c = 'o' ->
          scn.token_stream <<- Octlit (take_while is_oct_tail scn.char_stream |> implode, scn.prec_counter); scan ((++)scn)
        | Some c when c = 'B' || c = 'b' ->
          scn.token_stream <<- Binlit (take_while is_bin_tail scn.char_stream |> implode, scn.prec_counter); scan ((++)scn)
        | Some c when c = 'E' || c = 'e' ->
          scn.token_stream <<- Extlit (take_while is_ext_tail scn.char_stream |> implode, scn.prec_counter); scan ((++)scn)
        | Some _ -> raise Wrong_symbol
        | None -> raise Premature_eof
      in
      match_pound scn
    | Some _ -> raise Wrong_symbol
end
