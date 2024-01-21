let parseLine = line => {
  line
  |> String.splitList(~delimiter=" ")
  |> List.map(intStr =>
       intStr
       |> Int.fromString
       |> Result.fromOption({j|Failed to parse "$intStr"|j})
     )
  |> List.Result.sequence;
};

let pairUp = (lst: list(int)) => {
  let rec loop = (lst, acc) =>
    switch (lst) {
    | []
    | [_] => acc |> List.reverse
    | [x, y, ...rest] => loop([y, ...rest], [(x, y), ...acc])
    };

  loop(lst, []);
};

let diffs = (lst: list(int)) =>
  lst |> pairUp |> List.map(((x, y)) => y - x);

let makeChain = (startList: list(int)) => {
  let allZeroes = lst => lst |> List.filter(x => x != 0) |> List.length == 0;

  let rec loop = (acc: list(list(int)), lst) => {
    let diffs = lst |> diffs;

    if (diffs |> allZeroes) {
      [diffs, ...acc] |> List.reverse |> List.cons(startList);
    } else {
      loop([diffs, ...acc], diffs);
    };
  };

  loop([], startList);
};

let getNextValue = (lst: list(int)) => {
  lst
  |> makeChain
  |> List.map(List.last >> Result.fromOption("Failed to get last element!"))
  |> List.Result.sequence
  |> Result.map(List.foldLeft((+), 0));
};

let getPreviousValue = (lst: list(int)) => {
  lst
  |> makeChain
  |> List.map(List.head >> Result.fromOption("Failed to get head element!"))
  |> List.Result.sequence
  |> Result.map(List.foldRight((-), 0));
};

let doWork = partFunc =>
  String.splitList(~delimiter="\n")
  >> List.map(parseLine >> Result.flatMap(partFunc))
  >> List.Result.sequence
  >> Result.map(List.Int.sum)
  >> Result.fold(makeErrorMessage, Int.toString);

let doPart1 = doWork(getNextValue);

let doPart2 = doWork(getPreviousValue);

let p1TestInput = Day09Data.testInput;

let p2TestInput = Day09Data.testInput;

let actualInput = Day09Data.actualInput;

let doSandbox = None;

let sandboxInput = None;
