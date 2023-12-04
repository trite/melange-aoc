module IntMap = {
  include Map.WithOrd(Int.Ord);
};

type card = {
  id: int,
  winningNumbers: list(int),
  cardNumbers: list(int),
};

// Inputs contain extra spaces for formatting, but it's easier to parse without them
let removeExtraSpaces =
  String.replaceRegex(~search=[%re "/ +/g"], ~replaceWith=" ");

let parseNumbers = (failureString: string) =>
  String.splitList(~delimiter=" ")
  >> List.map(String.toInt >> Result.fromOption(failureString))
  >> List.Result.sequence;

let parseCard = (cardString: string): result(card, string) => {
  (
    switch (cardString |> String.splitList(~delimiter=": ")) {
    | [idRaw, restRaw] => Ok((idRaw, restRaw))
    | _ => Error("Failed to separate id from rest")
    }
  )
  |> Result.flatMap(((idRaw, restRaw)) =>
       switch (idRaw |> String.splitList(~delimiter=" ")) {
       | ["Card", idRaw] =>
         switch (idRaw |> String.toInt) {
         | Some(id) => Ok((id, restRaw))
         | None => Error("Failed to parse id")
         }
       | _ => Error("Failed to separate \"Card\" from id")
       }
     )
  |> Result.flatMap(((id, restRaw)) =>
       switch (restRaw |> String.splitList(~delimiter=" | ")) {
       | [winningNumbersRaw, cardNumbersRaw] =>
         Ok((id, winningNumbersRaw, cardNumbersRaw))
       | _ => Error("Failed to separate winning numbers from card numbers")
       }
     )
  |> Result.flatMap(((id, winningNumbersRaw, cardNumbersRaw)) => {
       winningNumbersRaw
       |> parseNumbers("Failed to parse winning numbers")
       |> Result.map(winningNumbers => (id, winningNumbers, cardNumbersRaw))
     })
  |> Result.flatMap(((id, winningNumbers, cardNumbersRaw)) => {
       cardNumbersRaw
       |> parseNumbers("Failed to parse card numbers")
       |> Result.map(cardNumbers => {id, winningNumbers, cardNumbers})
     });
};

let doesCardContainWinningNumber =
    (winningNumber: int, cardNumbers: list(int)) =>
  cardNumbers |> List.Int.contains(winningNumber);

let calculatePoints = (listLength: int) =>
  switch (listLength) {
  | 0 => 0
  | _ =>
    listLength
    - 1
    |> List.repeat(_, 2)
    |> List.foldLeft((acc, curr) => acc * curr, 1)
  };

let doPart1 =
  removeExtraSpaces
  >> String.splitList(~delimiter="\n")
  >> List.map(
       parseCard
       >> Result.map(({winningNumbers, cardNumbers, _}: card) =>
            winningNumbers
            |> List.filter(winningNumber =>
                 doesCardContainWinningNumber(winningNumber, cardNumbers)
               )
            |> List.length
            |> calculatePoints
          ),
     )
  >> List.Result.sequence
  >> Result.fold(err => "Error: " ++ err, List.Int.sum >> Int.toString);

type cardTotals = {
  cardValue: int,
  cardTotal: int,
};

let updateCounts =
    (
      gameMap: IntMap.t(cardTotals),
      startFrom: int,
      stopAt: int,
      increaseBy: int,
    ) => {
  let rec go =
          (acc: IntMap.t(cardTotals), curr: int, stop: int, amount: int)
          : IntMap.t(cardTotals) =>
    if (curr == stop) {
      acc;
    } else {
      go(
        acc
        |> IntMap.update(
             curr,
             Option.map(({cardValue, cardTotal}) =>
               {cardValue, cardTotal: cardTotal + amount}
             ),
           ),
        curr + 1,
        stop,
        amount,
      );
    };

  go(gameMap, startFrom, stopAt, increaseBy);
};

let countTotal = (gameMap: IntMap.t(cardTotals)): result(int, string) => {
  let rec go =
          (acc: result(IntMap.t(cardTotals), string), curr: int, stop: int)
          : result(IntMap.t(cardTotals), string) =>
    if (curr > stop) {
      acc;
    } else {
      acc
      |> Result.flatMap(
           IntMap.get(curr) >> Result.fromOption("Failed to get card"),
         )
      |> Result.flatMap(({cardValue, cardTotal}) => {
           go(
             acc
             |> Result.map(gameMap =>
                  updateCounts(
                    gameMap,
                    curr + 1,
                    curr + cardValue + 1,
                    cardTotal,
                  )
                ),
             curr + 1,
             stop,
           )
         });
    };

  let stop = gameMap |> IntMap.length;

  go(Ok(gameMap), 1, stop)
  |> Result.map(
       IntMap.toList
       >> List.map(((_id, {cardTotal, _}: cardTotals)) => cardTotal)
       >> List.Int.sum,
     );
};

// Verify test data gets the right answer when using countTotal
assert(
  [
    (1, {cardValue: 4, cardTotal: 1}),
    (2, {cardValue: 2, cardTotal: 1}),
    (3, {cardValue: 2, cardTotal: 1}),
    (4, {cardValue: 1, cardTotal: 1}),
    (5, {cardValue: 0, cardTotal: 1}),
    (6, {cardValue: 0, cardTotal: 1}),
  ]
  |> IntMap.fromList
  |> countTotal
  |> Result.getOk
  |> Shared.Option.getOrFailWith("Assertion failure - couldn't get result")
  == 30,
);

let doPart2 =
  removeExtraSpaces
  >> String.splitList(~delimiter="\n")
  >> List.map(
       parseCard
       >> Result.map(({id, winningNumbers, cardNumbers}: card) =>
            winningNumbers
            |> List.filter(winningNumber =>
                 doesCardContainWinningNumber(winningNumber, cardNumbers)
               )
            |> List.length
            |> (x => (id, {cardValue: x, cardTotal: 1}))
          ),
     )
  >> List.Result.sequence
  >> Result.flatMap(IntMap.fromList >> countTotal)
  >> Result.fold(err => "Error: " ++ err, Int.toString);

let p1TestInput = Day04Data.testInput;

let p2TestInput = Day04Data.testInput;

let actualInput = Day04Data.actualInput;
