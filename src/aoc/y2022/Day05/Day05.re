type move = {
  count: int,
  start: string,
  destination: string,
};

let parseInput = {
  let parseLayoutAndMoves = ((layout, moves: list(string))) => {
    let parseLayoutLine = line => {
      let rec go = (acc, rest) =>
        rest
        |> (
          fun
          | [_lb, x, _rb, _space, ...r] => go([x, ...acc], r)
          | [_lb, x, _rb] => [x, ...acc] |> List.reverse
          | _ =>
            raise(
              Failure(
                "Unable to parse layout line. Line: ["
                ++ (rest |> List.map(String.make) |> List.String.join)
                ++ "]",
              ),
            )
        );

      line |> String.toList |> go([]);
    };

    let parseLayout =
      List.map(parseLayoutLine)
      >> Shared.List.transpose
      >> List.map(
           List.reverse
           >> List.filter(x => x != " ")
           >> List.uncons
           >> Option.getOrThrow
           >> (((x, lst)) => (x, lst |> List.reverse)),
         )
      >> String.Map.fromList;

    let parseMove =
      fun
      | ["move", count, "from", start, "to", destination] => {
          count: count |> Int.fromString |> Option.getOrThrow,
          start,
          destination,
        }
      | _ => raise(Failure("Unable to parse move!"));

    let parseMoves = List.map(String.splitList(~delimiter=" ") >> parseMove);

    (layout |> parseLayout, moves |> parseMoves);
  };

  String.splitList(~delimiter="\n")
  >> Shared.List.split(~delimiter="")
  >> Shared.List.toTuple2
  >> parseLayoutAndMoves;
};

let applyMovesPart1 = ((layout, moves)) => {
  let moveItem = (layout, start, destination) => {
    let (toMove, startStack) =
      layout
      |> Map.get(start)
      |> Option.flatMap(List.uncons)
      |> Shared.Option.getOrFailWith("Unable to get item to move!");

    let destinationStack =
      layout
      |> Map.get(destination)
      |> Shared.Option.getOrFailWith(
           "Unable to get destination stack! Destination: " ++ destination,
         )
      |> List.cons(toMove);

    layout
    |> Map.update(start, _ => Some(startStack))
    |> Map.update(destination, _ => Some(destinationStack));
  };

  let rec applyMove = (layout, move) =>
    switch (move) {
    | {count: 0, start: _, destination: _} => layout
    | {count: x, start, destination} =>
      applyMove(
        moveItem(layout, start, destination),
        {count: x - 1, start, destination},
      )
    };

  moves |> List.foldLeft(applyMove, layout);
};

let applyMovesPart2 = ((layout, moves)) => {
  let applyMove = (layout, {count, start, destination}) => {
    let (toMove, startStack) =
      layout
      |> Map.get(start)
      |> Option.flatMap(List.splitAt(count))
      |> Shared.Option.getOrFailWith("Unable to split stack!");

    let destinationStack =
      layout
      |> Map.get(destination)
      |> Shared.Option.getOrFailWith("Unable to get destination stack!")
      |> List.concat(toMove);

    layout
    |> Map.update(start, _ => Some(startStack))
    |> Map.update(destination, _ => Some(destinationStack));
  };

  moves |> List.foldLeft(applyMove, layout);
};

let grabResult =
  Map.toArray
  >> Array.map(Tuple.second >> List.head >> Option.getOrThrow)
  >> Array.String.join;

let doWork = (fApplyMoves, data) =>
  data |> parseInput |> fApplyMoves |> grabResult;

let doPart1 = doWork(applyMovesPart1);

let doPart2 = doWork(applyMovesPart2);

let p1TestInput = Day05Data.testInput;

let p2TestInput = Day05Data.testInput;

let actualInput = Day05Data.actualInput;
