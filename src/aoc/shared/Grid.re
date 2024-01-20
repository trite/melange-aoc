module IntMap = {
  include Map.WithOrd(Int.Ord);
};

type t = IntMap.t(IntMap.t(string));

type coordRange = {
  start_: Coord.t,
  end_: Coord.t,
};

let get: (Coord.t, t) => option(string) =
  ({x, y}, grid) =>
    grid |> IntMap.get(y) |> Option.flatMap(IntMap.get(x));

/**
  Expects a block of text where each char is its own cell,
    and each line is a row of cells, separated by line returns.
*/
let fromStringBlock: string => t =
  String.splitList(~delimiter="\n")
  >> List.mapWithIndex((row, i) =>
       (
         i,
         row
         |> String.toList
         |> List.mapWithIndex((char, j) => (j, char))
         |> IntMap.fromList,
       )
     )
  >> IntMap.fromList;

let findByValue: (string, t) => list(Coord.t) =
  (value, grid) =>
    grid
    |> IntMap.toList
    |> List.map(((y, row)) =>
         row
         |> IntMap.toList
         |> List.filter(((_x, str)) => str == value)
         |> List.map(((x, _)) => ({x, y}: Coord.t))
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
    : list((Coord.t, option(string))) =>
  range(yStart, yEnd + 1)
  |> List.map(y =>
       range(xStart, xEnd + 1)
       |> List.map(x => {
            let coord: Coord.t = {x, y};
            (coord, get(coord, grid));
          })
     )
  |> List.flatten;

let getAllCoords = grid =>
  grid
  |> IntMap.toArray
  |> Array.map(((y, row)) =>
       row |> IntMap.toArray |> Array.map(((x, _)) => ({x, y}: Coord.t))
     )
  |> Array.flatten;

let getRowAtY: (t, int) => option(list((Coord.t, string))) =
  (grid, y) =>
    grid
    |> IntMap.get(y)
    |> Option.map(
         IntMap.toList >> List.map(((x, str)) => ({x, y}: Coord.t, str)),
       );

let getRowAtYValues: (t, int) => option(list(string)) =
  (grid, y) => getRowAtY(grid, y) |> Option.map(List.map(Tuple.second));

let getColAtX: (t, int) => option(list((Coord.t, string))) =
  (grid, x) =>
    grid
    |> IntMap.toList
    |> List.map(((y, row)) =>
         row |> IntMap.get(x) |> Option.map(str => ({x, y}: Coord.t, str))
       )
    |> List.Option.sequence;

let getColAtYValues: (t, int) => option(list(string)) =
  (grid, x) => getColAtX(grid, x) |> Option.map(List.map(Tuple.second));
