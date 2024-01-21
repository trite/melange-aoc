let expandUniverse = grid => {
  let rows = grid |> Shared.Grid.getRowsWithId;

  let emptyRows =
    rows
    |> List.map(((i, str)) =>
         if (str |> String.contains(~search="#")) {
           None;
         } else {
           Some((i, str));
         }
       )
    |> List.catOptions
    |> List.reverse;

  let emptyCols =
    grid
    |> Shared.Grid.getColsWithId
    |> List.map(((i, str)) =>
         if (str |> String.contains(~search="#")) {
           None;
         } else {
           let strLen = str |> String.length;
           // rowLen = row length after expansion
           // need to match this before adding to grid
           // or get errors since row lengths don't match
           let rowLen = List.length(rows) + List.length(emptyRows);
           let diff = rowLen - strLen;
           Js.log2("diff", diff);
           Some((i, str ++ String.repeat(diff, ".")));
         }
       )
    |> List.catOptions
    |> List.reverse;

  Js.log2("emptyRows", emptyRows |> List.toArray);
  Js.log2("emptyCols", emptyCols |> List.toArray);

  grid
  |> Result.pure
  |> List.foldLeft(
       (grid, (i, str)) =>
         grid
         |> Result.flatMap(Shared.Grid.insertRowAt(i, str |> String.toList)),
       _,
       emptyRows,
     )
  |> List.foldLeft(
       (grid, (i, str)) =>
         grid
         |> Result.flatMap(Shared.Grid.insertColAt(i, str |> String.toList)),
       _,
       emptyCols,
     );
};

// Test out the universe expansion function
assert(
  Day11Data.testInputP1
  |> Shared.Grid.fromStringBlock
  |> expandUniverse
  |> Result.fold(
       _ => false,
       grid =>
         grid |> Shared.Grid.toString === Day11Data.testInputP1ExpandedUniverse,
     ),
);

let manhattanDistance =
    ({x: x1, y: y1}: Shared.Coord.t, {x: x2, y: y2}: Shared.Coord.t) =>
  abs(x2 - x1) + abs(y2 - y1);

// Not tail recursive, too much recursion with actual data
// let rec uniquePairs = list =>
//   switch (list) {
//   | [] => []
//   | [_] => []
//   | [x, ...xs] =>
//     let pairs = List.map(y => (x, y), xs);
//     let restPairs = uniquePairs(xs);
//     List.append(pairs, restPairs);
//   };
// let uniquePairs = list => {
// };

// Tail recursive
let uniquePairs: list('a) => list(('a, 'a)) =
  list => {
    let rec go = (acc, list) =>
      switch (list) {
      | [] => acc
      | [x, ...xs] =>
        let pairs = List.map(y => (x, y), xs);
        go(List.concat(pairs, acc), xs);
      };
    go([], list);
  };

let doPart1 =
  Shared.Grid.fromStringBlock
  >> expandUniverse
  >> Result.map(
       Shared.Grid.findByValue("#")
       >> uniquePairs
       >> List.foldLeft(
            (acc, (c1, c2)) =>
              acc
              |> Shared.CoordPair.Map.set(
                   {c1, c2}: Shared.CoordPair.t,
                   manhattanDistance(c1, c2),
                 ),
            Shared.CoordPair.Map.make(),
          )
       >> Shared.CoordPair.Map.toArray
       >> Array.map(Tuple.second)
       >> Array.Int.sum,
     )
  >> Shared.Result.mapWithErrorText(Js.Json.stringifyAny >> Option.getOrThrow);

let doPart2 = _ => "NYI";

let p1TestInput = Day11Data.testInputP1;

let p2TestInput = "Not there yet";

let actualInput = Day11Data.actualInput;

let doSandbox = None;

let sandboxInput = None;
