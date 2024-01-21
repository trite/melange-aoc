// TODO: Part 2 could use some optimization (takes about 3-5s to run), but leaving for now

type raceResult = {
  time: float,
  distance: float,
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
         |> Float.fromString
         |> Result.fromOption(
              "Failed to parse `" ++ name ++ "` value: " ++ time,
            )
       )
    |> List.Result.sequence
  | _ => Error("Failed to split `" ++ name ++ "` line on \": \"")
  };

let p1Parse =
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

let range = (start_: float, end_: float): list(float) => {
  let startInt = start_ |> Float.toInt;
  let endInt = end_ |> Float.toInt;
  List.makeWithIndex(endInt - startInt, i => start_ +. (i |> Float.fromInt));
};

let countPossibleVictories = ({time: maxTime, distance: minDistance}) =>
  range(1., maxTime)
  |> List.filter(timeHeld => timeHeld *. (maxTime -. timeHeld) > minDistance)
  |> List.length;

assert(countPossibleVictories({time: 7., distance: 9.}) == 4);
assert(countPossibleVictories({time: 15., distance: 40.}) == 8);
assert(countPossibleVictories({time: 30., distance: 200.}) == 9);

let doPart1 =
  p1Parse
  >> Result.map(
       List.map(countPossibleVictories) >> List.foldLeft(( * ), 1),
     )
  >> Shared.Result.mapWithErrorText(Int.toString);

let p2ParseLine = (line: string, name: string) =>
  switch (line |> String.splitList(~delimiter=": ")) {
  | [lineName, xs] when lineName == name =>
    let xs =
      xs |> String.replaceRegex(~search=[%re "/ +/g"], ~replaceWith="");

    xs
    |> Float.fromString
    |> Result.fromOption("Failed to parse `" ++ name ++ "` value: " ++ xs);
  | _ => Error("Failed to split `" ++ name ++ "` line on \": \"")
  };

let p2Parse =
  removeExtraSpaces
  >> String.splitList(~delimiter="\n")
  >> (
    fun
    | [timeLine, distanceLine] => Ok((timeLine, distanceLine))
    | _ => Error("Need exactly 2 lines of input!")
  )
  >> Result.flatMap(((timeLine, distanceLine)) => {
       p2ParseLine(timeLine, "Time")
       |> Result.map(time => (time, distanceLine))
     })
  >> Result.flatMap(((time, distanceLine)) => {
       p2ParseLine(distanceLine, "Distance")
       |> Result.map(distance => {time, distance})
     });

let doPart2 =
  p2Parse
  >> Result.map(countPossibleVictories)
  >> Shared.Result.mapWithErrorText(Int.toString);

let p1TestInput = Day06Data.testInput;

let p2TestInput = Day06Data.testInput;

let actualInput = Day06Data.actualInput;

let doSandbox = None;

let sandboxInput = None;
