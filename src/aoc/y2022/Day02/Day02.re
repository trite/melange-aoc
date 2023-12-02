type move =
  | Rock
  | Paper
  | Scissors;

let moveToScore =
  fun
  | Rock => 1
  | Paper => 2
  | Scissors => 3;

type outcome =
  | Lose
  | Draw
  | Win;

let outcomeToScore =
  fun
  | Lose => 0
  | Draw => 3
  | Win => 6;

let parseMove =
  fun
  | "A"
  | "X" => Rock
  | "B"
  | "Y" => Paper
  | "C"
  | "Z" => Scissors
  | _ => raise(Failure("Cannot parse move"));

let parseString =
  String.splitList(~delimiter=" ")
  >> (
    fun
    | [a, b, ..._rest] => (a |> parseMove, b |> parseMove)
    | _ => raise(Failure("Cannot parse string"))
  );

let doWork = (calculation, data) =>
  data
  |> String.splitList(~delimiter="\n")
  |> List.map(parseString >> calculation)
  |> List.Int.sum
  |> Int.toString;

let part1 = ((opponent, self)) => {
  let result =
    switch (opponent, self) {
    | (Rock, Rock)
    | (Paper, Paper)
    | (Scissors, Scissors) => Draw
    | (Rock, Paper)
    | (Paper, Scissors)
    | (Scissors, Rock) => Win
    | (Rock, Scissors)
    | (Paper, Rock)
    | (Scissors, Paper) => Lose
    };

  (self |> moveToScore) + (result |> outcomeToScore);
};

let convertMoveToOutcome =
  fun
  | Rock => Lose
  | Paper => Draw
  | Scissors => Win;

let loseTo =
  fun
  | Rock => Scissors
  | Paper => Rock
  | Scissors => Paper;

let winAgainst =
  fun
  | Rock => Paper
  | Paper => Scissors
  | Scissors => Rock;

let part2 = ((opponent, self)) =>
  switch (self |> convertMoveToOutcome) {
  | Lose => (Lose |> outcomeToScore) + (opponent |> loseTo |> moveToScore)
  | Draw => (Draw |> outcomeToScore) + (opponent |> moveToScore)
  | Win => (Win |> outcomeToScore) + (opponent |> winAgainst |> moveToScore)
  };

let doPart1 = doWork(part1);

let doPart2 = doWork(part2);

let p1TestInput = Day02Data.testInput;

let p2TestInput = Day02Data.testInput;

let actualInput = Day02Data.actualInput;
