(* Not sure if I'll use the OCaml version, but it works: *)

let testInput =
  "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"


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
  List.reverse (go lst delim [])


let parse =
  String.splitList ~delimiter:"\n"
  >> splitList ""
  >> List.map
       ( List.map
           (Int.fromString >> Result.fromOption "Failed to parse input string")
       >> List.Result.sequence )
  >> List.Result.sequence


let doWork partFunc =
  parse >> Result.fold (fun err -> "Error: " ^ err) (partFunc >> Int.toString)


let part1 = List.maxBy Int.compare >> Option.getOrThrow

let part2 =
  List.sortBy (Int.compare |> Ord.reverse) >> List.take 3 >> List.Int.sum
