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

// let getDistanceFromTimeHeld = (timeHeld: int) =>
//   range(1, timeHeld) |> List.Int.sum;

// let getDistanceTraveled = (timeHeld: int, runTime: int) =>
//   getDistanceFromTimeHeld(timeHeld) * runTime;

// let _getPossibleDistances = (minDistance: int, maxTime: int) =>
//   range(1, maxTime)
//   |> List.map(x => getDistanceTraveled(x, maxTime - x))
//   |> List.filter(x => x >= minDistance);

// let getPossibleDistances = (maxTime: int) =>
//   range(1, maxTime) |> List.map(speed => speed * (maxTime - speed));

// let getPossibleWinningDistances = (~maxTime: int, ~minDistance: int, ()) =>
//   getPossibleDistances(maxTime) |> List.filter(x => x > minDistance);

// let countPossibleVictories = (~maxTime: int, ~minDistance: int, ()) =>
//   getPossibleWinningDistances(~maxTime, ~minDistance, ()) |> List.length;

let countPossibleVictories = (~maxTime: int, ~minDistance: int, ()) =>
  range(1, maxTime)
  |> List.filter(timeHeld => timeHeld * (maxTime - timeHeld) > minDistance)
  |> List.length;

assert(countPossibleVictories(~maxTime=7, ~minDistance=9, ()) == 4);
assert(countPossibleVictories(~maxTime=15, ~minDistance=40, ()) == 8);
assert(countPossibleVictories(~maxTime=30, ~minDistance=200, ()) == 9);

// let getPossibleDistances = (maxTime: int) =>
//   range(1, maxTime) |> List.map(getDistanceFromTimeHeld);

let doPart1 =
  parse
  >> Result.map(
       List.map(({time, distance}) =>
         countPossibleVictories(~maxTime=time, ~minDistance=distance, ())
       )
       >> List.foldLeft(( * ), 1),
     )
  >> Result.fold(
       err => "Error: " ++ err,
       Int.toString,
       //  List.toArray >> Js.Json.stringifyAny >> Option.getOrThrow,
     );

let doPart2 = _ => "Not yet implemented";

let p1TestInput = Day06Data.testInput;

let p2TestInput = "Not there yet";

let actualInput = Day06Data.actualInput;

let doSandbox = None;

let sandboxInput = None;

// // let doSandbox =
// //   Some(
// //     input =>
// //       input
// //       |> Int.fromString
// //       |> Result.fromOption("Failed to parse sandbox input as int")
// //       // |> Result.map(getDistanceFromTimeHeld >> Int.toString)
// //       |> Result.map(
// //            getPossibleDistances(9)
// //            >> Js.Json.stringifyAny
// //            >> Option.getOrThrow,
// //          )
// //       |> Result.fold(err => "Error: " ++ err, id),
// //   );

// let doSandbox =
//   Some(
//     _input =>
//       // getPossibleDistances(7)
//       // countPossibleVictories(~maxTime=7, ~minDistance=9, ())
//       // countPossibleVictories(~maxTime=15, ~minDistance=40, ())
//       // countPossibleVictories(~maxTime=30, ~minDistance=200, ())
//       countPossibleVictories(~maxTime=30, ~minDistance=200, ())
//       |> List.toArray
//       |> Js.Json.stringifyAny
//       |> Option.getOrThrow,
//   );

// let sandboxInput = Some("");
