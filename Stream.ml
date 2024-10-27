module Stream = struct
  type t = 'a Seq.t ref

  exception Empty_stream
  exception Consume_failed

  let empty = ref Seq.empty

  let of_list lst = ref (List.to_seq lst)

  let is_spent stm = Seq.is_empty !stm

  let dup stm =
    let new_stream = empty in
    Seq.iter (fun i -> new_stream <- i) !stm;
    new_stream

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
    let rec aux acc n' =
      if n = 0 then List.rev acc
      else ((next stm') :: acc) (decr n)

  let peek_opt stm =
    match !stm () with
    | Seq.Nil -> None
    | Seq.Cons (hd, _) -> Some hd

  let skip_next stm =
    let _ = next stm in
    stm

  let remaining stm = Seq.length !stm

  let append stm lst = 
    stm := Seq.append !stm (List.to_seq lst)

  let (<<-) i stm = 
    append stm [i]

  let (>>) stm =
    skip_next stm

  let take_while pred stm =
    let rec aux acc stm' =
      match peek_opt stm' with
      | Some i when pred i -> aux (i :: acc) (>> stm')
      | Some _ -> List.rev acc
      | _ -> raise Consume_failed
    in
    aux [] stm

  let take_until pred stm =
    let rec aux acc stm' =
      match peek_opt stm' with
      | Some i when pred i -> List.rev acc
      | Some i -> aux (i :: acc) (>> stm')
      | _ -> raise Consume_failed
    in
    aux [] stm

  let drop_while pred stm =
    let _ = take_while pred stm in
    ()

  let drop_until pred stm =
    let _ = take_until pred stm in
    ()
end
