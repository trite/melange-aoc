module IntMap = {
  include Map.WithOrd(Int.Ord);
};

type t = IntMap.t(IntMap.t(string));

// TODO: move coord stuff to a module, ex:
// module Coord = {
//   type t = {
//     x: int,
//     y: int,
//   };

//   let eq = ({x: x1, y: y1}, {x: x2, y: y2}) => x1 == x2 && y1 == y2;
// };

type coord = {
  x: int,
  y: int,
};

let coordEq = ({x: x1, y: y1}, {x: x2, y: y2}) => x1 == x2 && y1 == y2;

let coordToString = ({x, y}) =>
  "(" ++ Int.toString(x) ++ ", " ++ Int.toString(y) ++ ")";

type coordRange = {
  start_: coord,
  end_: coord,
};

let get: (coord, t) => option(string) =
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

let findByValue: (string, t) => list(coord) =
  (value, grid) =>
    grid
    |> IntMap.toList
    |> List.map(((y, row)) =>
         row
         |> IntMap.toList
         |> List.filter(((_x, str)) => str == value)
         |> List.map(((x, _)) => {x, y})
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
    : list((coord, option(string))) =>
  range(yStart, yEnd + 1)
  |> List.map(y =>
       range(xStart, xEnd + 1)
       |> List.map(x => {
            let coord = {x, y};
            (coord, get(coord, grid));
          })
     )
  |> List.flatten;
