[@ocaml.warning "-69"]
type raceResult = {
  time: int,
  distance: int,
};

let makeRaceResult = ((time, distance)) => {time, distance};

let removeExtraSpaces =
  String.replaceRegex(~search=[%re "/ +/g"], ~replaceWith=" ");

let parseLine = (line: string, name: string) =>
  switch (line |> String.splitList(~delimiter=": ")) {
  | [lineName, xs] when lineName == name =>
    xs
    |> String.splitList(~delimiter=" ")
    |> List.map(time =>
         time
         |> Int.fromString
         |> Result.fromOption(
              "Failed to parse `" ++ name ++ "` value: " ++ time,
            )
       )
    |> List.Result.sequence
  | _ => Error("Failed to split `" ++ name ++ "` line on \": \"")
  };

let parse =
  removeExtraSpaces
  >> String.splitList(~delimiter="\n")
  >> (
    fun
    | [timeLine, distanceLine] => Ok((timeLine, distanceLine))
    | _ => Error("Need exactly 2 lines of input!")
  )
  >> Result.flatMap(((timeLine, distanceLine)) =>
       parseLine(timeLine, "Time")
       |> Result.map(times => (times, distanceLine))
     )
  >> Result.flatMap(((times, distanceLine)) =>
       parseLine(distanceLine, "Distance")
       |> Result.map(distances => (times, distances))
     )
  >> Result.map(((times, distances)) =>
       List.zip(times, distances) |> List.map(makeRaceResult)
     );

let range = (start_: int, end_: int): list(int) =>
  List.makeWithIndex(end_ - start_, i => start_ + i);

let _getDistanceFromTimeHeld = (timeHeld: int) =>
  range(1, timeHeld + 1) |> List.Int.sum;

let doPart1 =
  parse
  >> Result.fold(
       err => "Error: " ++ err,
       List.toArray >> Js.Json.stringifyAny >> Option.getOrThrow,
     );

let doPart2 = _ => "Not yet implemented";

let p1TestInput = Day06Data.testInput;

let p2TestInput = "Not there yet";

let actualInput = Day06Data.actualInput;
