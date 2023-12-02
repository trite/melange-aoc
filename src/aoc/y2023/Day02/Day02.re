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

let parseLine = line => {
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
                 | [x, "blue"] =>
                   x
                   |> Int.fromString
                   |> Result.fromOption("Failed to parse blue count")
                   |> Result.map(blue)
                 | [x, "green"] =>
                   x
                   |> Int.fromString
                   |> Result.fromOption("Failed to parse green count")
                   |> Result.map(green)
                 | [x, "red"] =>
                   x
                   |> Int.fromString
                   |> Result.fromOption("Failed to parse red count")
                   |> Result.map(red)
                 | _ => Error("Failed to parse game part")
                 }
               )
            |> List.Result.sequence
          )
       |> List.Result.sequence
       |> Result.map(gameInfo => {gameNumber, gameInfo})
     });
};

let maxRed = 12;
let maxGreen = 13;
let maxBlue = 14;

let isGamePossible = game => {
  game.gameInfo
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
    | 0 => Some(game.gameNumber)
    | _ => None
  );
};

let doPart1 =
  String.splitList(~delimiter="\n")
  >> List.map(parseLine >> Result.map(isGamePossible))
  >> List.Result.sequence
  >> Result.map(List.catOptions >> List.Int.sum)
  >> Result.fold(err => err, x => x |> Int.toString);

let doPart2 = _ => "Not yet implemented";

let p1TestInput = Day02Data.testInput;

let p2TestInput = Day02Data.testInput;

let actualInput = Day02Data.actualInput;
