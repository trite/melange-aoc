type card =
  | Joker
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Queen
  | King
  | Ace;

let cardToInt =
  fun
  | Joker => 0
  | Two => 2
  | Three => 3
  | Four => 4
  | Five => 5
  | Six => 6
  | Seven => 7
  | Eight => 8
  | Nine => 9
  | Ten => 10
  | Queen => 12
  | King => 13
  | Ace => 14;

let stringToCard =
  fun
  | "J" => Ok(Joker)
  | "2" => Ok(Two)
  | "3" => Ok(Three)
  | "4" => Ok(Four)
  | "5" => Ok(Five)
  | "6" => Ok(Six)
  | "7" => Ok(Seven)
  | "8" => Ok(Eight)
  | "9" => Ok(Nine)
  | "T" => Ok(Ten)
  | "Q" => Ok(Queen)
  | "K" => Ok(King)
  | "A" => Ok(Ace)
  | x => Error("Invalid card: " ++ x);

let cardToString =
  fun
  | Joker => "J"
  | Two => "2"
  | Three => "3"
  | Four => "4"
  | Five => "5"
  | Six => "6"
  | Seven => "7"
  | Eight => "8"
  | Nine => "9"
  | Ten => "T"
  | Queen => "Q"
  | King => "K"
  | Ace => "A";

let oneOfEachCard = [
  Joker,
  Two,
  Three,
  Four,
  Five,
  Six,
  Seven,
  Eight,
  Nine,
  Ten,
  Queen,
  King,
  Ace,
];

let compareCards = (cardA, cardB) =>
  Int.compare(cardToInt(cardA), cardToInt(cardB));

type handCards = list(card);

type handType =
  | HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind;

let handTypeToInt =
  fun
  | HighCard => 1
  | OnePair => 2
  | TwoPair => 3
  | ThreeOfAKind => 4
  | FullHouse => 5
  | FourOfAKind => 6
  | FiveOfAKind => 7;

let getHandType = (cards: handCards) => {
  let emptyMap =
    oneOfEachCard
    |> List.map(card => (card |> cardToString, 0))
    |> StringMap.fromList;

  let fullMap =
    cards
    |> List.map(cardToString)
    |> List.foldLeft(
         (acc, card) =>
           acc |> StringMap.update(card, Option.map(count => count + 1)),
         emptyMap,
       );

  let jokerCount = fullMap |> StringMap.get("J") |> Option.getOrElse(0);

  fullMap
  |> StringMap.remove("J")
  |> StringMap.toList
  |> List.map(((_, count)) => count)
  |> List.filter(count => count > 0)
  |> List.sortBy(Int.compare)
  |> List.reverse
  |> List.mapWithIndex((count, i) =>
       if (i == 0) {
         count + jokerCount;
       } else {
         count;
       }
     )
  |> (
    fun
    | [] => FiveOfAKind
    | [5] => FiveOfAKind
    | [4, ..._] => FourOfAKind
    | [3, 2] => FullHouse
    | [3, ..._] => ThreeOfAKind
    | [2, 2, ..._] => TwoPair
    | [2, ..._] => OnePair
    | _ => HighCard
  );
};

let compareCardLists = (cardListA, cardListB) => {
  let rec go = cardsList =>
    switch (cardsList) {
    | [] => raise(Failure("Shouldn't happen"))
    | [(cardA, cardB), ...rest] =>
      switch (compareCards(cardA, cardB)) {
      | `less_than => `less_than
      | `greater_than => `greater_than
      | `equal_to => go(rest)
      }
    };

  go(List.zip(cardListA, cardListB));
};

let compareHands = (handA, handB) =>
  Int.compare(
    handA |> getHandType |> handTypeToInt,
    handB |> getHandType |> handTypeToInt,
  )
  |> (
    fun
    | `less_than => `less_than
    | `greater_than => `greater_than
    | `equal_to => compareCardLists(handA, handB)
  );

type handAndBid = {
  hand: handCards,
  bid: int,
};

// for debugging only
let _handAndBidToString = ({hand, bid}) =>
  hand
  |> List.map(cardToString)
  |> List.String.join
  |> (handStr => handStr ++ " " ++ Int.toString(bid));

let compareHandAndBids = (handAndBidA, handAndBidB) =>
  compareHands(handAndBidA.hand, handAndBidB.hand);

let parseLabelsToHandCards = (labels: string) =>
  labels
  |> String.toList
  |> List.map(stringToCard)
  |> List.Result.sequence
  |> Result.flatMap(handCards =>
       switch (handCards) {
       | [_, _, _, _, _] => Ok(handCards)
       | _ => Error("Invalid number of cards")
       }
     );

let doWork =
  String.splitList(~delimiter="\n")
  >> List.map(line =>
       line
       |> String.splitList(~delimiter=" ")
       |> (
         fun
         | [handRaw, bidRaw] => Ok((handRaw, bidRaw))
         | _ => Error("Line didn't have exactly 2 values")
       )
       |> Result.flatMap(((handRaw, bidRaw)) =>
            handRaw
            |> parseLabelsToHandCards
            |> Result.map(hand => (hand, bidRaw))
          )
       |> Result.flatMap(((hand, bidRaw)) =>
            bidRaw
            |> Int.fromString
            |> Result.fromOption("Failed to parse bid")
            |> Result.map(bid => {hand, bid})
          )
     )
  >> List.Result.sequence
  >> Result.map(
       List.sortBy(compareHandAndBids)
       >> List.mapWithIndex(({hand: _, bid}, index) => {bid * (index + 1)})
       >> List.Int.sum,
     )
  >> Shared.Result.mapWithErrorText(Int.toString);
