type card = {
  // Id isn't used in part 1, but will be in part 2, ignoring warning for now
  // Can either use an attribute or prefix id with _ to do this, I prefer the latter
  // [@ocaml.warning "-69"]
  _id: int,
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
       |> Result.map(cardNumbers => {_id: id, winningNumbers, cardNumbers})
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

let doPart2 = id;

let p1TestInput = Day04Data.testInput;

let p2TestInput = Day04Data.testInput;

let actualInput = Day04Data.actualInput;
