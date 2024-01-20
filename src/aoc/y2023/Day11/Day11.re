// Get coords of each `#` in the grid
let doPart1 =
  Shared.Grid.fromStringBlock
  >> Shared.Grid.findByValue("#")
  >> List.map(Shared.Coord.toString)
  >> List.String.joinWith("\n");

let doPart2 = _ => "NYI";

let p1TestInput = Day11Data.testInput;

let p2TestInput = Day11Data.testInput;

let actualInput = Day11Data.actualInput;

let doSandbox = None;

let sandboxInput = None;
