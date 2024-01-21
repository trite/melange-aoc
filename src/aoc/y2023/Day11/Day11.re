// Get coords of each `#` in the grid
// let doPart1 =
//   Shared.Grid.fromStringBlock
//   >> Shared.Grid.findByValue("#")
//   >> List.map(Shared.Coord.toString)
//   >> List.String.joinWith("\n");

// type emptyRowsAndCols = {
//   emptyRows: list((int, string)),
//   emptyCols: list((int, string)),
// };

// let getEmptyRowsAndColumns = grid => {
//   let emptyRows =
//     grid
//     |> Shared.Grid.getRowsWithId
//     |> List.map(((i, str)) =>
//          if (str |> String.contains(~search="#")) {
//            None;
//          } else {
//            Some((i, str));
//          }
//        )
//     |> List.catOptions;

//   let emptyCols =
//     grid
//     |> Shared.Grid.getColsWithId
//     |> List.map(((i, str)) =>
//          if (str |> String.contains(~search="#")) {
//            None;
//          } else {
//            Some((i, str));
//          }
//        )
//     |> List.catOptions;

//   {emptyRows, emptyCols};
// };

let expandUniverse = grid => {
  let emptyRows =
    grid
    |> Shared.Grid.getRowsWithId
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
           Some((i, str));
         }
       )
    |> List.catOptions
    |> List.reverse;

  grid
  |> Result.pure
  |> List.foldLeft(
       (grid, (i, str)) =>
         grid |> Shared.Grid.insertRowAt(i, str |> String.toList),
       _,
       emptyRows,
     )
  |> List.foldLeft(
       (grid, (i, str)) =>
         grid |> Shared.Grid.insertColAt(i, str |> String.toList),
       _,
       emptyCols,
     )
  |> ignore; // TODO: Remove this

  // emptyRows |> List.foldLeft()

  Js.log2("emptyRows", emptyRows |> List.toArray);
  Js.log2("emptyCols", emptyCols |> List.toArray);

  // TODO: just returning the original grid for now to do some data inspection
  //       still need to order the empty rows/cols in descending order based on `i` given `((i, str))`
  //       and then insert them in that order (should be able to use `s/Shared.Grid.insert(Row|Col)At/` for this)
  grid;
  // grid
  // |> Shared.Grid.insert
  // let grid =
  // emptyRows
  // |> List.map(((i, str)) => )
};

let doPart1 =
  Shared.Grid.fromStringBlock >> expandUniverse >> Shared.Grid.toString;

let doPart2 = _ => "NYI";

let p1TestInput = Day11Data.testInput;

let p2TestInput = Day11Data.testInput;

let actualInput = Day11Data.actualInput;

let doSandbox = None;

let sandboxInput = None;
