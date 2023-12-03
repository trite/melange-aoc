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

let get = (x: int, y: int, grid: grid): string =>
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
    (xStart: int, xEnd: int, yStart: int, yEnd: int, grid: grid)
    : list(list(string)) =>
  range(yStart, yEnd + 1)
  |> List.map(y => range(xStart, xEnd + 1) |> List.map(x => get(x, y, grid)));

let doPart1 =
  parseToGrid
  >> getRange(0, 0, 0, 0)
  >> List.map(List.toArray)
  >> List.toArray
  >> Js.Json.stringifyAny
  >> Option.getOrThrow;
// >> getRange(0, 0, 2, 0)
// // range(0, 5)
// // |> List.map(y =>
// //      range(0, 5) |> List.map(x => {j|$x$y|j}) |> List.String.joinWith(",")
// //    )
// // |> List.String.joinWith("\n");
// >> Js.Json.stringifyAny
// >> Option.getOrThrow;

let doPart2 = id;

let p1TestInput = Day03Data.testInput;

let p2TestInput = "";

let actualInput = Day03Data.actualInput;
