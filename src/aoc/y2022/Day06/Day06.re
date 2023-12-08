// let parseTestData =
//   String.splitList(~delimiter="\n")
//   >> List.map(String.splitList(~delimiter=" ") >> Shared.List.toTuple3);

let (^::) = List.cons;

let tails = lst => {
  let rec go = lst =>
    switch (lst) {
    | [] => []
    | x =>
      let newX = x |> List.drop(1);
      newX ^:: go(newX);
    };

  lst ^:: go(lst);
};

// TODO: need a filterMap implementation to make this less ugly
let windows = (n, xs) =>
  List.map(List.takeExactly(n), tails(xs))
  |> List.filter(Option.isSome)
  |> List.map(Option.getOrThrow);

let unique = lst => {
  let oLen = lst |> List.length;
  let nLen = lst |> List.distinctBy(String.eq) |> List.length;
  oLen == nLen;
};

let firstUniqueWithCount = (x, lst) => {
  let rec go = (lst, count) =>
    switch (lst) {
    | [] =>
      raise(Failure("Reached end of list without finding anything unique!"))
    | [subList, ...rest] =>
      unique(subList) ? x + count : go(rest, count + 1)
    };

  go(lst, 0);
};

let doWork = (count, s) =>
  s
  |> String.toList
  |> windows(count)
  |> firstUniqueWithCount(count)
  |> Int.toString;

let doPart1 = doWork(4);

let doPart2 = doWork(14);

let doSandbox = None;

let p1TestInput = Day06Data.testInput;

let p2TestInput = Day06Data.testInput;

let actualInput = Day06Data.actualInput;

let sandboxInput = None;
