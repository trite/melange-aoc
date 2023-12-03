/*
    TODO:
      * Get list of all number groups as coord ranges, ex:
        "467" = 0,0 through 2,0
        "114" = 5,0 through 7,0
        "35"  = 2,2 through 3,2

      * Get range surrounding any of these coord ranges, ex:
        "467" = -1,-1 through 3,1
        "114" = 4,-1 through 8,1
        "35"  = 1,1 through 4,3

      * Look at entire top and bottom row of ranges, and first/last char on middle row
        All ranges should be 3 rows tall, and 2 characters wider than the number range
 */

module IntMap = {
  include Map.WithOrd(Int.Ord);
};

type grid = IntMap.t(IntMap.t(string));

let parseToGrid = (input: string): grid =>
  input
  |> String.splitList(~delimiter="\n")
  |> List.mapWithIndex((row, i) =>
       (
         i,
         row
         |> String.toList
         |> List.mapWithIndex((char, j) => (j, char))
         |> IntMap.fromList,
       )
     )
  |> IntMap.fromList;

type coord = {
  x: int,
  y: int,
};

type coordRange = {
  start_: coord,
  end_: coord,
};

let get = ({x, y}: coord, grid: grid): string =>
  switch (IntMap.get(y, grid)) {
  | None => "."
  | Some(row) =>
    switch (IntMap.get(x, row)) {
    | None => "."
    | Some(str) => str
    }
  };

let range = (start_: int, end_: int): list(int) =>
  List.makeWithIndex(end_ - start_, i => start_ + i);

let getRange =
    (
      {start_: {x: xStart, y: yStart}, end_: {x: xEnd, y: yEnd}}: coordRange,
      grid: grid,
    )
    : list(list(string)) =>
  range(yStart, yEnd + 1)
  |> List.map(y =>
       range(xStart, xEnd + 1) |> List.map(x => get({x, y}, grid))
     );

let doPart1 =
  parseToGrid
  >> getRange({
       start_: {
         x: 2,
         y: 2,
       },
       end_: {
         x: 3,
         y: 2,
       },
     })
  >> List.map(List.toArray)
  >> List.toArray
  >> Js.Json.stringifyAny
  >> Option.getOrThrow;

let doPart2 = id;

let p1TestInput = Day03Data.testInput;

let p2TestInput = "";

let actualInput = Day03Data.actualInput;
