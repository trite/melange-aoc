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

type findingNumber =
  | FindingEnd(int)
  | FindingStart;

let (^::) = List.cons;

let findNumbers = (row: list(string)) => {
  let rec go =
          (
            acc: list((int, int)),
            finding: findingNumber,
            position: int,
            rest: list(string),
          ) =>
    switch (rest) {
    | [] =>
      switch (finding) {
      | FindingEnd(x) => (x, position - 1) ^:: acc
      | FindingStart => acc
      }
    | [char, ...rest] =>
      switch (finding) {
      | FindingEnd(x) =>
        if (char >= "0" && char <= "9") {
          go(acc, FindingEnd(x), position + 1, rest);
        } else {
          go((x, position - 1) ^:: acc, FindingStart, position + 1, rest);
        }
      | FindingStart =>
        if (char >= "0" && char <= "9") {
          go(acc, FindingEnd(position), position + 1, rest);
        } else {
          go(acc, FindingStart, position + 1, rest);
        }
      }
    };

  go([], FindingStart, 0, row);
};

let getAround =
    (
      {start_: {x: xStart, y: yStart}, end_: {x: xEnd, y: yEnd}}: coordRange,
      grid: grid,
    ) =>
  getRange(
    {
      start_: {
        x: xStart - 1,
        y: yStart - 1,
      },
      end_: {
        x: xEnd + 1,
        y: yEnd + 1,
      },
    },
    grid,
  );

let isSymbol = (char: string) => (char < "0" || char > "9") && char != ".";

assert("." |> isSymbol == false);
assert("0" |> isSymbol == false);
assert("5" |> isSymbol == false);
assert("9" |> isSymbol == false);
assert("*" |> isSymbol == true);
assert("@" |> isSymbol == true);
assert("=" |> isSymbol == true);

let isPartNumber =
  List.mapWithIndex((row, i) =>
    switch (i) {
    | 0
    | 2 =>
      row |> List.map(isSymbol) |> List.foldLeft((a, b) => a || b, false)
    | 1 =>
      row
      |> List.mapWithIndex((char, j) =>
           (j == 0 || j == List.length(row) - 1) && isSymbol(char)
         )
      |> List.foldLeft((a, b) => a || b, false)
    | _ => raise(Failure("NYI"))
    }
  )
  >> List.foldLeft((a, b) => a || b, false);

assert(
  [
    [".", ".", ".", ".", "."],
    [".", "1", "2", "3", "."],
    [".", ".", ".", ".", "."],
  ]
  |> isPartNumber == false,
);

assert(
  [
    [".", ".", ".", "#", "."],
    [".", "1", "2", "3", "."],
    [".", ".", ".", ".", "."],
  ]
  |> isPartNumber == true,
);

assert(
  [
    [".", ".", ".", ".", "@"],
    [".", "1", "2", "3", "."],
    [".", ".", ".", ".", "."],
  ]
  |> isPartNumber == true,
);

let doPart1 = (input: string) => {
  let grid = parseToGrid(input);

  input
  |> String.splitList(~delimiter="\n")
  |> List.mapWithIndex((row, i) =>
       row
       |> String.toList
       |> findNumbers
       |> List.map(((start_, end_)) => {
            let coordRange = {
              start_: {
                x: start_,
                y: i,
              },
              end_: {
                x: end_,
                y: i,
              },
            };

            if (getAround(coordRange, grid) |> isPartNumber) {
              Some(
                getRange(coordRange, grid)
                |> List.map(List.foldLeft((a, b) => a ++ b, ""))
                |> List.foldLeft((a, b) => a ++ b, ""),
              );
            } else {
              None;
            };
          })
       |> List.catOptions
       |> List.map(x =>
            x |> Int.fromString |> Result.fromOption("Could not parse int")
          )
       |> List.Result.sequence
       |> Result.map(List.foldLeft((a, b) => a + b, 0))
     )
  |> List.Result.sequence
  |> Result.fold(err => "Error: " ++ err, List.Int.sum >> Int.toString);
};

let doPart2 = id;

let p1TestInput = Day03Data.testInput;

let p2TestInput = Day03Data.testInput;

let actualInput = Day03Data.actualInput;
