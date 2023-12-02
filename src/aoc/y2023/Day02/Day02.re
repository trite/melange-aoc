[@deriving accessors]
type colorCount =
  | Blue(int)
  | Green(int)
  | Red(int);

type round = list(colorCount);

type game = {
  gameNumber: int,
  gameInfo: list(round),
};

let parseMapInt = (name, toMap, i) =>
  i
  |> Int.fromString
  |> Result.fromOption("Failed to parse " ++ name ++ " count")
  |> Result.map(toMap);

let parseLine = line =>
  // Split game number from game info
  (
    switch (line |> String.splitList(~delimiter=": ")) {
    | [gameNumberRaw, gameInfoRaw] => Ok((gameNumberRaw, gameInfoRaw))
    | _ => Error("Failed to separate game number from game info")
    }
  )
  // Parse game number
  |> Result.flatMap(((gameNumberRaw, gameInfoRaw)) => {
       switch (gameNumberRaw |> String.splitList(~delimiter=" ")) {
       | [_, numberRaw] =>
         numberRaw
         |> Int.fromString
         |> Option.fold(Error("Failed to parse game number"), x =>
              Ok((x, gameInfoRaw))
            )
       | _ => Error("Failed to split game number from \"Game\" prefix")
       }
     })
  // Parse game info
  |> Result.flatMap(((gameNumber, gameInfoRaw)) => {
       gameInfoRaw
       |> String.splitList(~delimiter="; ")
       |> List.map(gameRaw =>
            String.splitList(~delimiter=", ", gameRaw)
            |> List.map(gamePartRaw =>
                 switch (gamePartRaw |> String.splitList(~delimiter=" ")) {
                 | [x, "blue"] => x |> parseMapInt("blue", blue)
                 | [x, "green"] => x |> parseMapInt("green", green)
                 | [x, "red"] => x |> parseMapInt("red", red)
                 | _ => Error("Failed to parse game part")
                 }
               )
            |> List.Result.sequence
          )
       |> List.Result.sequence
       |> Result.map(gameInfo => {gameNumber, gameInfo})
     });

let maxRed = 12;
let maxGreen = 13;
let maxBlue = 14;

let isGamePossible = ({gameNumber, gameInfo}) =>
  gameInfo
  |> List.map(round =>
       round
       |> List.map(colorCount =>
            switch (colorCount) {
            | Blue(x) when x > maxBlue => false
            | Blue(_) => true
            | Green(x) when x > maxGreen => false
            | Green(_) => true
            | Red(x) when x > maxRed => false
            | Red(_) => true
            }
          )
       |> List.keep(x => x == false)
       |> List.length
       |> (x => x == 0)
     )
  |> List.keep(x => x == false)
  |> List.length
  |> (
    fun
    | 0 => Some(gameNumber)
    | _ => None
  );

let doPart1 =
  String.splitList(~delimiter="\n")
  >> List.map(parseLine >> Result.map(isGamePossible))
  >> List.Result.sequence
  >> Result.map(List.catOptions >> List.Int.sum)
  >> Result.fold(err => err, x => x |> Int.toString);

type p2Round = {
  blue: int,
  green: int,
  red: int,
};

// This assumes no rounds have more than 1 count for the same color in a list
// A list of [Blue(1), Blue(3)] would be treated as just Blue(3)
let makeP2Round = (round: round) => {
  let updateP2Round = (p2Round, colorCount) =>
    switch (colorCount) {
    | Blue(blue) => {...p2Round, blue}
    | Green(green) => {...p2Round, green}
    | Red(red) => {...p2Round, red}
    };

  round
  |> List.foldLeft(
       (p2Round, colorCount) => updateP2Round(p2Round, colorCount),
       {blue: 0, green: 0, red: 0},
     );
};

let getMaxValues = (rounds: list(p2Round)) =>
  rounds
  |> List.foldLeft(
       (acc, curr) => {
         {
           blue: Int.max(acc.blue, curr.blue),
           green: Int.max(acc.green, curr.green),
           red: Int.max(acc.red, curr.red),
         }
       },
       {blue: 0, green: 0, red: 0},
     );

let doPart2 =
  String.splitList(~delimiter="\n")
  >> List.map(
       parseLine
       >> Result.map(({gameInfo, _}: game) =>
            gameInfo
            |> List.map(makeP2Round)
            |> getMaxValues
            |> (x => x.blue * x.green * x.red)
          ),
     )
  >> List.Result.sequence
  >> Result.map(List.Int.sum)
  >> Result.fold(err => err, x => x |> Int.toString);

let p1TestInput = Day02Data.testInput;

let p2TestInput = Day02Data.testInput;

let actualInput = Day02Data.actualInput;
