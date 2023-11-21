let testInput =
  "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"


(* let (>>) f g x = g (f x)

   (* let parse = String.split_on_char '\n' >> List.map Belt.Int.fromString >>
   Belt.List.keepMap (Belt.Option.mapWithDefault 0 (fun x -> x)) *) let parse =
   String.

   let testing123 = parse testInput *)

let splitList delim lst =
  let rec go lst delim acc =
    match lst with
    | [] ->
        acc
    | h :: t -> (
        if h = delim then go t delim ([] :: acc)
        else
          match acc with
          | [] ->
              go t delim [[h]]
          | h2 :: t2 ->
              go t delim ((h :: h2) :: t2) )
  in
  Stdlib.List.rev (go lst delim [])


let intOptionListToIntListResult lst =
  let rec go lst acc =
    match lst with
    | [] ->
        Belt.Result.Ok (Stdlib.List.rev acc)
    | h :: t -> (
      match h with
      | None ->
          Belt.Result.Error "Failed to parse input string"
      | Some x ->
          go t (x :: acc) )
  in
  go lst []


let intOptionListListToIntListListResult lst =
  let rec go lst acc =
    match lst with
    | [] ->
        Belt.Result.Ok (Stdlib.List.rev acc)
    | h :: t -> (
      match h with
      | Belt.Result.Error x ->
          Belt.Result.Error x
      | Belt.Result.Ok x ->
          go t (x :: acc) )
  in
  go lst []


let optionToResult x =
  match x with
  | None ->
      Belt.Result.Error "Failed to parse input string"
  | Some x ->
      Belt.Result.Ok x


(* let parse x = let firstSplit = String.split_on_char '\n' x in let secondSplit
   = splitList firstSplit "" in List.map (List.map Belt.Int.fromString)
   secondSplit *)
(* let fromStr = List.map (List.map Belt.Int.fromString) secondSplit in let next
   = intOptionListListToIntListListResult fromStr *)

(* let ( >> ) f g x = g (f x) *)

let ( >> ) = Relude.Globals.( >> )

let parse x =
  x
  |> Stdlib.String.split_on_char '\n'
  |> splitList ""
  |> List.map
       (List.map (fun z -> z |> Belt.Int.fromString |> Belt.Option.getExn))


let parse2 =
  (* Stdlib.String.split_on_char '\n' *)
  String.splitList ~delimiter:"\n"
  >> splitList ""
  >> List.map (List.map (Belt.Int.fromString >> Belt.Option.getExn))
