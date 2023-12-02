let parse =
  String.splitArray(~delimiter=",")
  >> Array.map(
       String.splitArray(~delimiter="-")
       >> Array.map(Int.fromString >> Option.getOrThrow)
       >> Shared.Array.toTuple2,
     )
  >> Shared.Array.toTuple2;

let fullyContains = ((a, b)) => {
  let doesAContainB = (((a1, a2), (b1, b2))) => a1 <= b1 && a2 >= b2;

  doesAContainB((a, b)) || doesAContainB((b, a));
};

let overlaps = (((a1, a2), (b1, b2))) => {
  a1 <= b2 && a2 >= b1 || b1 <= a2 && b2 >= a1;
};

let doWork = (comparison, data) =>
  data
  |> String.splitArray(~delimiter="\n")
  |> Array.filter(parse >> comparison)
  |> Array.count
  |> Int.toString;

let doPart1 = doWork(fullyContains);

let doPart2 = doWork(overlaps);

let p1TestInput = Day04Data.testInput;

let p2TestInput = Day04Data.testInput;

let actualInput = Day04Data.actualInput;
