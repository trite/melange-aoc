type emptyRowsAndCols = {
  emptyRows: list((int, string)),
  emptyCols: list((int, string)),
};

let getEmptyRowsAndColumns = grid => {
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

  {emptyRows, emptyCols};
};

let expandUniverse = grid => {
  let {emptyRows, emptyCols} = getEmptyRowsAndColumns(grid);

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
       >> Array.map((({c1, c2}: Shared.CoordPair.t, dist)) => {
            let c1 = Shared.Coord.toString(c1);
            let c2 = Shared.Coord.toString(c2);
            {j|{ "c1": $c1, "c2": $c2, "dist": $dist, }|j};
          })
       >> Array.String.joinWith(",\n"),
       //  >> Array.map(Tuple.second),
       //  >> Array.Int.sum,
     )
  >> Shared.Result.mapWithErrorText(id);
// >> Shared.Result.mapWithErrorText(Js.Json.stringifyAny >> Option.getOrThrow);

/*
   Strategy:
     Making the expanded universe section 1,000,000 times larger
       than the original will be computationally silly, since
       the grid stores things as a Map of Maps.

     For part 2's "expand universe" step try getting the list of
       rows and columns to expand, then simply returning those.

     The function for counting distance will need to check how many
       of these "expanded" rows/cols are crossed by a line between
       two points, and then add `the multiplier` to the total.

     `The multiplier` should be scalable so I can set it to 10 and
       100 and verify the correct results come out.

     The result for a 10x multiplier should be 1030
     The result for a 100x multiplier should be 8410
 */

// let doPart2 = _ => "NYI";

// let getMultiplier = (baseMultiplier: int, )

// let p2ManhattanDistance =
//     (
//       {x: x1, y: y1}: Shared.Coord.t,
//       {x: x2, y: y2}: Shared.Coord.t,
//       baseMultiplier: int,
//       emptyRowsAndCols: emptyRowsAndCols,
//     ) =>
//   manhattanDistance(
//     {x: x1, y: y1},
//     {
//       x:
//         x2
//         + (
//           emptyRowsAndCols.emptyCols
//           |> List.filter(((i, _)) => i < x2 && i > x1 || i < x1 && i > x2)
//           |> List.length
//         )
//         * baseMultiplier,

//       y:
//         y2
//         + (
//           emptyRowsAndCols.emptyRows
//           |> List.filter(((i, _)) => i < y2 && i > y1 || i < y1 && i > y2)
//           |> List.length
//         )
//         * baseMultiplier,
//     },
//   );

// let expandUniverse2

let doPart2 = inputStr => {
  let grid = inputStr |> Shared.Grid.fromStringBlock;

  let emptyRowsAndCols = getEmptyRowsAndColumns(grid);

  let baseMultiplier = 2;

  grid
  |> Shared.Grid.findByValue("#")
  |> List.map(({x, y}: Shared.Coord.t) =>
       (
         {
           x:
             x
             + (
               emptyRowsAndCols.emptyCols
               |> List.filter(((i, _)) => i < x)
               |> List.length
             )
             * (baseMultiplier - 1),

           y:
             y
             + (
               emptyRowsAndCols.emptyRows
               |> List.filter(((i, _)) => i < y)
               |> List.length
             )
             * (baseMultiplier - 1),
         }: Shared.Coord.t
       )
     )
  |> uniquePairs
  |> List.foldLeft(
       (acc, (c1, c2)) =>
         acc
         |> Shared.CoordPair.Map.set(
              {c1, c2}: Shared.CoordPair.t,
              // p2ManhattanDistance(c1, c2, baseMultiplier, emptyRowsAndCols),
              manhattanDistance(c1, c2),
            ),
       Shared.CoordPair.Map.make(),
     )
  |> Shared.CoordPair.Map.toArray
  // |> Array.map(Tuple.second)
  |> Array.map((({c1, c2}: Shared.CoordPair.t, dist)) => {
       let c1 = Shared.Coord.toString(c1);
       let c2 = Shared.Coord.toString(c2);
       {j|{ "c1": $c1, "c2": $c2, "dist": $dist, }|j};
     })
  |> Array.String.joinWith(",\n");
  // |> Js.Json.stringifyAny
  // |> Option.getOrThrow;
  // |> Array.Int.sum
  // |> Int.toString;
};

let p1TestInput = Day11Data.testInputP1;

let p2TestInput = Day11Data.testInputP1;

let actualInput = Day11Data.actualInput;

let doSandbox = None;

let sandboxInput = None;
