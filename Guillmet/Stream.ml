module Stream : sig
  type 'a t

  exception Empty_stream
  exception Consume_failed
  exception Peek_failed
  exception Single_skip_failed

  val of_list : 'a list -> 'a t
  val is_spent : 'a t -> bool
  val append : 'a t -> 'a list -> unit
  val (<<-) : 'a -> 'a t -> unit
  val dup : 'a t -> 'a t
  val rev : 'a t -> 'a t

  val peek : 'a t -> 'a
  val next : 'a t -> 'a
  val peek_last : 'a t -> 'a
  val npeek : 'a t -> int -> 'a list
  val peek_opt : 'a t -> 'a option
  val npeek_safe : 'a t -> int -> 'a list option
  val skip_next : 'a t -> 'a t

  val remaining : 'a t -> int

  val take_while : ('a -> bool) -> 'a t -> 'a list
  val take_until : ('a -> bool) -> 'a t -> 'a list
  val drop_while : ('a -> bool) -> 'a t -> unit
  val drop_until : ('a -> bool) -> 'a t -> unit
  val skip_single : ('a -> bool) -> 'a t -> 'a
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

  let (<<-) i stm = 
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
    | Some c when pred c -> next stm
    | Some _ -> raise Single_skip_failed
    | None -> raise Empty_stream
end
