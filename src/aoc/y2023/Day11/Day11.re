// Get coords of each `#` in the grid
// let doPart1 =
//   Shared.Grid.fromStringBlock
//   >> Shared.Grid.findByValue("#")
//   >> List.map(Shared.Coord.toString)
//   >> List.String.joinWith("\n");

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

/*
   TODO:
     - Get coords of all `#` in grid
     - Write function to count steps from one `#` to another
       * abs(x2 - x1) + abs(y2 - y1) + maybe some off-by-1 correction(s)
     - Write function to run the above function for every pair of `#` in the grid
     - Sum it all up
 */
let doPart1 =
  Shared.Grid.fromStringBlock
  >> expandUniverse
  >> Shared.Result.mapWithErrorText(Shared.Grid.toString);

let doPart2 = _ => "NYI";

let p1TestInput = Day11Data.testInputP1;

let p2TestInput = "Not there yet";

let actualInput = Day11Data.actualInput;

let doSandbox = None;

let sandboxInput = None;
