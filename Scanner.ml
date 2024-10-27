module Scanner = struct
  type t =
    { token_stream : token Stream.t
    ; char_stream  : char Stream.t
    }

  and token =
    | Ident of string
    | Symlit of string
    | Strlit of string
    | Charlit of string
    | Intlit of string
    | Reallit of string
    | Ldelim of char
    | Rdelim of char

  exception Scan_error

  open Stream

  let empty = { token_stream = Stream.empty ; char_stream = Stream.empty }

  let rec scan scn =
    match peek_opt scn.char_stream with
    | Some c when is_ldelim c ->
      scn.token_stream <<- Ldelim (>> scn.char_stream); scan scn
    | Some c when is_rdelim c ->
      scn.token_stream <<- Rdelim (>> scn.char_stream); scan scn
    | Some c when is_id_head c ->
      scn.token_stream <<- Ident (take_while is_id_tail scn.char_stream |> implode); scan scn
    | Some c when is_int_head c ->
      scn.token_stream <<- Intlit (take_while is_int_tail scn.char_stream |> implode |> Int64.of_string)
    | Some c when is_real_head c ->
      scn.token_stream <<- Reallit (take_while is_real_tail scn.char_stream |> implode |> Float.of_string)
    | Some c when is_char_head c ->
      scn.token_stream <<- Charlit (take_while is_char_tail scn.char_stream |> implode)
    | _ -> raise Scan_error

  (* Scanner still has some -- a lot to be exact; work to do *)

end
