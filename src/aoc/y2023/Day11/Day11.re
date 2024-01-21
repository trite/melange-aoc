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
  Day11Data.testInput
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
       >> Array.Int.sum
       >> Int.toString,
     )
  >> Shared.Result.mapWithErrorText(id);

/*
   This version "expands the universe" by finding the applicable rows/cols
     and multiplying them by the base multiplier (-1 to avoid off-by-one errors).

   The grid is never rendered after this point, lest the universe expand
     to the point where the computer explodes.

   In fact, floats are used because this will overflow the Int32 max limit.
 */
let expandUniverse2 =
    (
      emptyRowsAndCols: emptyRowsAndCols,
      baseMultiplier: float,
      {x, y}: Shared.Coord.t,
    ) => (
  Float.fromInt(x)
  +. (
    emptyRowsAndCols.emptyCols
    |> List.filter(((i, _)) => i < x)
    |> List.length
    |> Float.fromInt
  )
  *. (baseMultiplier -. 1.0),
  Float.fromInt(y)
  +. (
    emptyRowsAndCols.emptyRows
    |> List.filter(((i, _)) => i < y)
    |> List.length
    |> Float.fromInt
  )
  *. (baseMultiplier -. 1.0),
);

let manhattanDistanceFloat = (~x1: float, ~y1: float, ~x2: float, ~y2: float) =>
  Float.abs(x2 -. x1) +. Float.abs(y2 -. y1);

/*
   Part 2 strategy:
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

let part2Work = (baseMultiplier, inputStr) => {
  let grid = inputStr |> Shared.Grid.fromStringBlock;

  let emptyRowsAndCols = getEmptyRowsAndColumns(grid);

  grid
  |> Shared.Grid.findByValue("#")
  |> List.map(expandUniverse2(emptyRowsAndCols, baseMultiplier))
  |> uniquePairs
  |> List.foldLeft(
       (acc, ((x1, y1), (x2, y2))) => {
         acc
         |> Shared.CoordPair.Map.set(
              {
                c1: {
                  x: x1 |> Int.fromFloat,
                  y: y1 |> Int.fromFloat,
                },
                c2: {
                  x: x2 |> Int.fromFloat,
                  y: y2 |> Int.fromFloat,
                },
              },
              manhattanDistanceFloat(~x1, ~y1, ~x2, ~y2),
            )
       },
       Shared.CoordPair.Map.make(),
     )
  |> Shared.CoordPair.Map.toArray
  |> Array.map(Tuple.second)
  |> Array.Float.sum
  |> Float.toString;
};

let baseMultiplier = 1_000_000.0;

assert(part2Work(2.0, Day11Data.testInput) === "374");

assert(part2Work(10.0, Day11Data.testInput) === "1030");

assert(part2Work(100.0, Day11Data.testInput) === "8410");

assert(part2Work(baseMultiplier, Day11Data.testInput) === "82000210");

assert(part2Work(baseMultiplier, Day11Data.actualInput) === "483844716556");

let doPart2 = part2Work(baseMultiplier);

let p1TestInput = Day11Data.testInput;

let p2TestInput = Day11Data.testInput;

let actualInput = Day11Data.actualInput;

let doSandbox = None;

let sandboxInput = None;
