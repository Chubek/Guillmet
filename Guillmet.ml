module Stream = struct
  type 'a t = 'a Seq.t ref

  exception Empty_stream

  let empty = ref Seq.empty

  let peek stm =
    match !stm () with
    | Seq.Nil -> raise Empty_stream
    | Seq.Cons (hd, _) -> hd

  let npeek stm n =
    let rec aux acc stm' n' =
      match !stm' () with
      | Seq.Nil -> raise Empty_stream
      | Seq.Cons (hd, tl) -> 
        if n = 0
        then List.rev acc
        else aux (hd :: acc) tl (n' - 1)
    in
    aux [] stm n

  let next stm =
    match !stm () with
    | Seq.Nil -> raise Empty_stream
    | Seq.Cons (hd, tl) -> stm := tl; hd

  let take_while pred stm =
    let rec aux acc stm' =
      if pred (peek stm')
      then aux (next stm' :: acc) stm'
      else List.rev acc
    in
    aux [] stm

  let take_until pred stm =
    let rec aux acc stm' =
      if pred (peek stm')
      then List.rev acc
      else peek (next stm' :: acc) stm'
    in
    aux [] stm

  let discard_while pred stm =
   let _ = take_while pred stm in
   ()

  let discard_until pred stm =
   let _ = take_until pred stm in
   ()

  let map f stm = Seq.map f !stm
  let iter f stm = Seq.iter f !stm
  let filter p stm = Seq.filter p !stm

  let of_list lst = ref (List.to_seq lst)
  let of_array arr = ref (Array.to_seq arr)
  let of_seq seq = ref seq

  let append stm lst =
    stm := !stm @ lst

  let cons i stm =
    stm := i :: !stm

  let (<-) stm i = cons i stm
  let (::) i stm = coms i stm
  let (@) stm lst = append stm lst
end

module Lexeme = struct
   let explode str = List.of_seq (String.to_seq str)
   let implode lst = String.of_seq (List.to_seq str)
end

module Scanner = struct
  type t = 
    { input       : string
    ; file        : in_channel
    ; tokstream   : Token.t Stream.t
    ; charstream  : char Stream.t
    }

  let skip_whitespace scn =
    Stream.discard_while Lexeme.is_whitespace scn

  let scan_integer scn =
    let lst = Stream.take_while Lexeme.is_int_tail scn.charstream in
    scn.tokstream <- Token.Integer (Int64.of_string (Lexeme.implode lst))

  let scan_xinteger scn =
    let lst = Stream.take_while Lexeme.is_xint_tail scn.charstream in
    scn.tokstream <- Token.Integer (Int64.of_string "0x" ^ (Lexeme.implode lst))

  let scan_ointeger scn =
    let lst = Stream.take_while Lexeme.is_oint_tail scn.charstream in
    scn.tokstream <- Token.Integer (Int64.of_string "0o" ^ (Lexeme.implode lst))

  let scan_binteger scn =
    let lst = Stream.take_while Lexeme.is_bint_tail scn.charstream in
    scn.tokstream <- Token.Integer (Int64.of_string "0b" ^ (Lexeme.implode lst))

  let scan_ident scn =
    let lst = Stream.take_while Lexeme.is_ident_tail scn.charstream in
    scn.tokstream <- Token.Ident (Lexeme.implode lst)
end


