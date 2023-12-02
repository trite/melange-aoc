let isNumber =
  fun
  | "1" => true
  | "2" => true
  | "3" => true
  | "4" => true
  | "5" => true
  | "6" => true
  | "7" => true
  | "8" => true
  | "9" => true
  | _ => false;

let doPart1 =
  String.splitList(~delimiter="\n")
  >> List.map(line => {
       let first =
         line
         |> String.toList
         |> List.find(isNumber)
         |> Shared.Option.getOrFailWith("No number found");

       let last =
         line
         |> String.toList
         |> List.reverse
         |> List.find(isNumber)
         |> Shared.Option.getOrFailWith("No number found");

       {j|$first$last|j}
       |> Int.fromString
       |> Shared.Option.getOrFailWith("Could not parse number");
     })
  >> List.Int.sum
  >> Int.toString;

type findResult =
  | Success(int, list(string))
  | Failure(list(string))
  | End;

// NOTE: apparently "sevenine" is treated as "79" by the AoC creators
// starting characters that need to be cons'd back onto the lists:
//   o, t, f, s, e, n
// This is the reason for the strange re-adding of letters to "rest" lists below
let findNumber =
  fun
  | ["1", ...rest]
  | ["o", "n", "e", ...rest] => Success(1, ["e", ...rest])
  | ["2", ...rest]
  | ["t", "w", "o", ...rest] => Success(2, ["o", ...rest])
  | ["3", ...rest]
  | ["t", "h", "r", "e", "e", ...rest] => Success(3, ["e", ...rest])
  | ["4", ...rest]
  | ["f", "o", "u", "r", ...rest] => Success(4, rest)
  | ["5", ...rest]
  | ["f", "i", "v", "e", ...rest] => Success(5, ["e", ...rest])
  | ["6", ...rest]
  | ["s", "i", "x", ...rest] => Success(6, rest)
  | ["7", ...rest]
  | ["s", "e", "v", "e", "n", ...rest] => Success(7, ["n", ...rest])
  | ["8", ...rest]
  | ["e", "i", "g", "h", "t", ...rest] => Success(8, ["t", ...rest])
  | ["9", ...rest]
  | ["n", "i", "n", "e", ...rest] => Success(9, ["e", ...rest])
  | [_, ...rest] => Failure(rest)
  | [] => End;

let findNumbers = (line: string) => {
  let rec go = (line: list(string), acc: list(int)) =>
    switch (findNumber(line)) {
    | Success(number, rest) => go(rest, [number, ...acc])
    | Failure(rest) => go(rest, acc)
    | End => acc |> List.reverse
    };

  go(String.toList(line), []);
};

let doPart2 =
  String.splitList(~delimiter="\n")
  >> List.map(line => {
       let numbers = line |> findNumbers;

       let first =
         numbers
         |> List.head
         |> Shared.Option.getOrFailWith("Failed to find first");

       let last =
         numbers
         |> List.last
         |> Shared.Option.getOrFailWith("Failed to find second");

       {j|$first$last|j}
       |> Int.fromString
       |> Shared.Option.getOrFailWith("Could not parse number");
     })
  >> List.Int.sum
  >> Int.toString;

let p1TestInput = Day01Data.p1TestInput;

let p2TestInput = Day01Data.p2TestInput;

let actualInput = Day01Data.actualInput;
