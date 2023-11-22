let split = (~delimiter) =>
  List.foldLeft(
    (b, a) =>
      a == delimiter
        ? b |> List.append([])
        : b
          |> List.initOrEmpty
          |> List.append(
               b |> List.last |> Option.getOrElse([]) |> List.append(a),
             ),
    [[]],
  );

let toTuple2 =
  fun
  | [a, b] => (a, b)
  | _ => raise(Failure("Cannot convert list to Tuple2 - invalid input"));

let toTuple3 =
  fun
  | [a, b, c] => (a, b, c)
  | _ => raise(Failure("Cannot convert list to Tuple3 - invalid input"));

let unsafeHead = lst => lst |> List.head |> Option.getOrThrow;

let unsafeTail = lst => lst |> List.tail |> Option.getOrThrow;

/* TODO: Still have a hard time understanding this, should spend more time with it at some point

     From: https://stackoverflow.com/a/56599499

     OCaml implementation:
       let rec transpose =
         function
         | []
         | [] :: _ -> []
         | rows    ->
             List.map List.hd rows :: transpose (List.map List.tl rows)
   */
let rec transpose =
  fun
  | []
  | [[], ..._] => []
  | rows => [
      List.map(unsafeHead, rows),
      ...transpose(List.map(unsafeTail, rows)),
    ];
