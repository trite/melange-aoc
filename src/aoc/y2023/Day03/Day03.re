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

let containsTrue = List.foldLeft((a, b) => a || b, false);

let isPartNumber =
  List.mapWithIndex((row, i) =>
    switch (i) {
    | 0
    | 2 => row |> List.map(isSymbol) |> containsTrue
    | 1 =>
      row
      |> List.mapWithIndex((char, j) =>
           (j == 0 || j == List.length(row) - 1) && isSymbol(char)
         )
      |> containsTrue
    | _ => raise(Failure("NYI"))
    }
  )
  >> containsTrue;

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
                |> List.map(List.String.join)
                |> List.String.join,
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

let findStars = (input: list(list(string))) =>
  input
  |> List.mapWithIndex((row, y) =>
       row
       |> List.mapWithIndex((char, x) =>
            if (char == "*") {
              Some({x, y});
            } else {
              None;
            }
          )
       |> List.catOptions
     )
  |> List.foldLeft(List.concat, []);

type numberWithCoordRange = {
  number: int,
  range: coordRange,
};

type findingNumberWithCoordRange =
  | Building(int, string)
  | Searching;

let makeNumberWithCoordRange = (str: string, range: coordRange) =>
  str
  |> Int.fromString
  |> Result.fromOption("Failed to parse int")
  |> Result.map(number => {number, range});

let findNumbersWithCoordRange = (row: list(string), rowNumber: int) => {
  let rec go =
          (
            acc: list(result(numberWithCoordRange, string)),
            finding: findingNumberWithCoordRange,
            position: int,
            rest: list(string),
          ) =>
    switch (rest) {
    | [] =>
      switch (finding) {
      | Building(xStart, str) =>
        makeNumberWithCoordRange(
          str,
          {
            start_: {
              x: xStart,
              y: rowNumber,
            },
            end_: {
              x: position - 1,
              y: rowNumber,
            },
          },
        )
        ^:: acc
      | Searching => acc
      }
    | [char, ...rest] =>
      switch (finding) {
      | Building(xStart, str) =>
        if (char >= "0" && char <= "9") {
          go(acc, Building(xStart, str ++ char), position + 1, rest);
        } else {
          go(
            makeNumberWithCoordRange(
              str,
              {
                start_: {
                  x: xStart,
                  y: rowNumber,
                },
                end_: {
                  x: position - 1,
                  y: rowNumber,
                },
              },
            )
            ^:: acc,
            Searching,
            position + 1,
            rest,
          );
        }
      | Searching =>
        if (char >= "0" && char <= "9") {
          go(acc, Building(position, char), position + 1, rest);
        } else {
          go(acc, Searching, position + 1, rest);
        }
      }
    };

  go([], Searching, 0, row);
};

let findNumbersWithCoordRanges = (input: list(list(string))) =>
  input
  |> List.mapWithIndex((row, i) => findNumbersWithCoordRange(row, i))
  |> List.foldLeft(List.concat, []);

let nearRange =
    (
      {x, y}: coord,
      {start_: {x: xStart, y: yStart}, end_: {x: xEnd, y: yEnd}}: coordRange,
    ) =>
  x >= xStart - 1 && x <= xEnd + 1 && y >= yStart - 1 && y <= yEnd + 1;

let doPart2 = (input: string) => {
  let lsts =
    input |> String.splitList(~delimiter="\n") |> List.map(String.toList);

  let numbers = lsts |> findNumbersWithCoordRanges |> List.Result.sequence;

  lsts
  |> findStars
  |> List.map(star =>
       numbers
       |> Result.map(
            List.filter(({range, _}) => nearRange(star, range))
            >> List.map(({number, _}) => number)
            >> (
              fun
              | [x, y] => x * y
              | _ => 0
            ),
          )
     )
  |> List.Result.sequence
  |> Result.fold(err => "Error: " ++ err, List.Int.sum >> Int.toString);
};

let doSandbox = None;

let p1TestInput = Day03Data.testInput;

let p2TestInput = Day03Data.testInput;

let actualInput = Day03Data.actualInput;

let sandboxInput = None;
