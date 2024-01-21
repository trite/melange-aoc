module IntMap = {
  include Map.WithOrd(Int.Ord);
};

type t = IntMap.t(IntMap.t(string));

type coordRange = {
  start_: Shared__Coord.t,
  end_: Shared__Coord.t,
};

let get: (Shared__Coord.t, t) => option(string) =
  ({x, y}, grid) =>
    grid |> IntMap.get(y) |> Option.flatMap(IntMap.get(x));

let listToRow: list(string) => IntMap.t(string) =
  List.mapWithIndex((str, x) => (x, str)) >> IntMap.fromList;

let rowsToGrid: list(IntMap.t(string)) => t =
  List.mapWithIndex((row, i) => (i, row)) >> IntMap.fromList;

let rowToString: IntMap.t(string) => string =
  IntMap.toList >> List.map(Tuple.second) >> List.String.joinWith("");

let getHeight: t => int = grid => grid |> IntMap.length;

let getWidth: t => int =
  grid =>
    grid
    |> IntMap.toList
    |> List.map(((_y, row)) => row |> IntMap.length)
    |> List.foldLeft(Int.max, 0);

let getRowsWithId: t => list((int, string)) =
  IntMap.toList >> List.map(((y, row)) => (y, row |> rowToString));

let getRows: t => list(string) = getRowsWithId >> List.map(Tuple.second);

let transposeGrid: t => t =
  grid =>
    grid
    |> IntMap.toList
    |> List.map(((_y, row)) =>
         row |> IntMap.toList |> List.map(((_x, str)) => str)
       )
    |> Shared__List.transpose
    |> List.map(listToRow)
    |> rowsToGrid;

let getColsWithId: t => list((int, string)) = transposeGrid >> getRowsWithId;

let getCols = getColsWithId >> List.map(Tuple.second);

// |> List.map(rows => {
//      let nameLater =
//        rows |> List.map((({x, y}, str)): Coord.t => {x: y, y: x}, str);

//      ();
//    })
// // |> List.map(listToRow)
// |> IntMap.fromList;

// let getColsWithId =
//   IntMap.toList
//   >> List.map(
//     ((y, row)) =>
//       row
//       |> IntMap.toList
//       |> List.map(((x, str)) => ({x, y}: Coord.t, str))
//   )
//   >> Shared.List.transpose
//   >> List.map(
//     List.map(
//       (({x, y}, str)) =>
//     )
//   )
//
// >> List.map(((y, row)) =>
//     row
//     |> IntMap.toList
//     |> List.map(((x, str)) => )
//   )

/**
  Expects a block of text where each char is its own cell,
    and each line is a row of cells, separated by line returns.
*/
let fromStringBlock: string => t =
  String.splitList(~delimiter="\n")
  >> List.map(String.toList >> listToRow)
  >> rowsToGrid;

let toString = grid =>
  grid
  |> IntMap.toList
  |> List.map(
       Tuple.second
       >> IntMap.toList
       >> List.map(Tuple.second)
       >> List.String.joinWith(""),
     )
  |> List.String.joinWith("\n");

let findByValue: (string, t) => list(Shared__Coord.t) =
  (value, grid) =>
    grid
    |> IntMap.toList
    |> List.map(((y, row)) =>
         row
         |> IntMap.toList
         |> List.filter(((_x, str)) => str == value)
         |> List.map(((x, _)) => ({x, y}: Shared__Coord.t))
       )
    |> List.flatten;

let range = (start_: int, end_: int): list(int) =>
  List.makeWithIndex(end_ - start_, i => start_ + i);

let getRange =
    ({start_: {x: xStart, y: yStart}, end_: {x: xEnd, y: yEnd}}, grid)
    : list(list(option(string))) =>
  range(yStart, yEnd + 1)
  |> List.map(y =>
       range(xStart, xEnd + 1) |> List.map(x => get({x, y}, grid))
     );

let getRangeWithCoords =
    ({start_: {x: xStart, y: yStart}, end_: {x: xEnd, y: yEnd}}, grid)
    : list((Shared__Coord.t, option(string))) =>
  range(yStart, yEnd + 1)
  |> List.map(y =>
       range(xStart, xEnd + 1)
       |> List.map(x => {
            let coord: Shared__Coord.t = {x, y};
            (coord, get(coord, grid));
          })
     )
  |> List.flatten;

let getAllCoords = grid =>
  grid
  |> IntMap.toArray
  |> Array.map(((y, row)) =>
       row
       |> IntMap.toArray
       |> Array.map(((x, _)) => ({x, y}: Shared__Coord.t))
     )
  |> Array.flatten;

let getRowAtY: (int, t) => option(list((Shared__Coord.t, string))) =
  (y, grid) =>
    grid
    |> IntMap.get(y)
    |> Option.map(
         IntMap.toList
         >> List.map(((x, str)) => ({x, y}: Shared__Coord.t, str)),
       );

let getRowAtYValues: (int, t) => option(list(string)) =
  (y, grid) => getRowAtY(y, grid) |> Option.map(List.map(Tuple.second));

let getColAtX: (int, t) => option(list((Shared__Coord.t, string))) =
  (x, grid) =>
    grid
    |> IntMap.toList
    |> List.map(((y, row)) =>
         row
         |> IntMap.get(x)
         |> Option.map(str => ({x, y}: Shared__Coord.t, str))
       )
    |> List.Option.sequence;

let getColAtYValues: (int, t) => option(list(string)) =
  (x, grid) => getColAtX(x, grid) |> Option.map(List.map(Tuple.second));

// let insertRowAt: (t, int, list(string)) => t =
//   (grid, y, row) =>
//     grid
//     |> IntMap.toList
//     |> List.insertAt(y, (y, row |> listToRow))
//     |> IntMap.fromList;

let insertRowAt: (int, list(string), t) => result(t, string) =
  (y, newRow, grid) =>
    switch (newRow |> List.length, getRowAtY(y, grid)) {
    | (0, _) => Error("Cannot insert empty row")
    | (_, None) => Error("Target row does not exist in grid")
    | (l1, Some(l2)) when l1 == (l2 |> List.length) =>
      grid
      |> IntMap.toList
      // Bump up all row numbers after the insertion point
      |> List.map(((y', row')) => (y' >= y ? y' + 1 : y', row'))
      |> List.insertAt(y, (y, newRow |> listToRow))
      |> IntMap.fromList
      |> Result.pure
    | _ => Error("Row length does not match")
    };

let insertColAt: (int, list(string), t) => result(t, string) =
  (x, newCol, grid) =>
    switch (newCol |> List.length, getColAtX(x, grid)) {
    | (0, _) => Error("Cannot insert empty column")
    | (_, None) => Error("Target column does not exist in grid")
    | (l1, Some(l2)) when l1 == (l2 |> List.length) =>
      grid
      |> IntMap.toList
      |> List.map(((y, row)) =>
           newCol
           |> List.at(y)
           |> Result.fromOption("Could not find value at index")
           |> Result.map(str =>
                (
                  y,
                  row
                  |> IntMap.toList
                  // Bump up all column numbers after the insertion point
                  |> List.map(((x', str')) =>
                       (x' >= x ? x' + 1 : x', str')
                     )
                  |> List.insertAt(x, (x, str))
                  |> IntMap.fromList,
                )
              )
         )
      |> List.Result.sequence
      |> Result.map(IntMap.fromList)
    | _ => Error("Column length does not match")
    };

// Verify that insertRowAt and insertcolAt work as intended
assert(
  {|*****
*****
*****
*****
*****|}
  |> fromStringBlock
  |> insertRowAt(2, ["-", "-", "-", "-", "-"])
  |> Result.flatMap(insertColAt(2, ["|", "|", "|", "|", "|", "|"]))
  |> Result.fold(Shared__Globals.makeErrorMessage, toString)
  == {|**|***
**|***
--|---
**|***
**|***
**|***|},
);

// Verify that getRowAt works as intended after inserting things
assert(
  {|*****
*****
*****
*****
*****|}
  |> fromStringBlock
  |> insertRowAt(2, ["-", "-", "-", "-", "-"])
  |> Result.flatMap(insertColAt(2, ["|", "|", "|", "|", "|", "|"]))
  |> Result.map(
       getRowAtY(2)
       >> Option.map(List.map(Tuple.second) >> List.String.joinWith(""))
       >> Option.fold(false, str => str == "--|---"),
     )
  |> Result.fold(Shared__Globals.const(false), id),
);
