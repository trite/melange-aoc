let splitList = (~delimiter) =>
  List.foldLeft(
    (b, a) =>
      a == delimiter
        ? b |> List.append([])
        : b
          |> List.initOrEmpty
          |> List.append(
               b |> List.last |> Option.getOrElse([]) |> List.append(a),
             ),
    [[]],
  );

let parse =
  String.splitList(~delimiter="\n")
  >> splitList(~delimiter="")
  >> List.map(
       List.map(Int.fromString >> Option.getOrThrow) >> List.foldLeft((+), 0),
     );

let doWork = partFunc => parse >> partFunc >> Int.toString;

let part1 = List.maxBy(Int.compare) >> Option.getOrThrow;

let part2 =
  List.sortBy(Int.compare |> Ord.reverse) >> List.take(3) >> List.Int.sum;

let doPart1 = doWork(part1);

let doPart2 = doWork(part2);

let p1TestInput = Day01Data.testInput;

let p2TestInput = Day01Data.testInput;

let actualInput = Day01Data.actualInput;
