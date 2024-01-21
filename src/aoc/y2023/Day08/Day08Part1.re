type direction =
  | Left
  | Right;

type node = {
  left: string,
  right: string,
};

type directionsAndNodes = {
  directions: list(direction),
  nodes: StringMap.t(node),
  firstNodeName: string,
  lastNodeName: string,
};

let parseDirections =
  String.toList
  >> List.map(rawDirection =>
       switch (rawDirection) {
       | "L" => Ok(Left)
       | "R" => Ok(Right)
       | _ => Error("Invalid direction")
       }
     )
  >> List.Result.sequence;

let parseNode = (nodeRaw: string) =>
  nodeRaw
  |> String.toList
  |> (
    fun
    | ["(", l1, l2, l3, ",", " ", r1, r2, r3, ")"] =>
      Ok({
        left: [l1, l2, l3] |> List.String.join,
        right: [r1, r2, r3] |> List.String.join,
      })
    | _ => Error("Invalid node")
  );

let parseNodeLine = (nodeLine: string) =>
  nodeLine
  |> String.splitList(~delimiter=" = ")
  |> (
    fun
    | [nodeName, nodeRaw] =>
      nodeRaw |> parseNode |> Result.map(node => (nodeName, node))
    | _ => Error("Invalid node line")
  );

let parseNodes = (nodesRaw: list(string)) => {
  nodesRaw
  |> List.map(parseNodeLine)
  |> List.Result.sequence
  |> Result.map(StringMap.fromList);
};

// Recursive approach results in too much recursion
// let countStepsToEnd =
//     ({directions, nodes, firstNodeName, lastNodeName}: directionsAndNodes) => {
//   let rec loop = (currentNodeName, steps) =>
//     switch (currentNodeName) {
//     | name when name == lastNodeName => Ok(steps)
//     | name =>
//       nodes
//       |> StringMap.get(name)
//       |> Result.fromOption("Failed to find node")
//       |> Result.flatMap(node =>
//            switch (directions |> List.head) {
//            | Some(Left) => loop(node.left, steps + 1)
//            | Some(Right) => loop(node.right, steps + 1)
//            | None => Error("Ran out of directions")
//            // | None => loop
//            }
//          )
//     };
//   loop(firstNodeName, 0);
// };

let countStepsToEnd =
    ({directions, nodes, firstNodeName, lastNodeName}: directionsAndNodes) => {
  let stepCount = ref(0);
  let foundEnd = ref(false);
  let currentNode = ref(firstNodeName);
  let errorState = ref(Ok());
  let currentDirections = ref(directions);

  while (! foundEnd^ && errorState^ == Ok()) {
    let node = nodes |> StringMap.get(currentNode^);

    switch (currentDirections^ |> List.head, node) {
    | (Some(Left), Some(node)) =>
      stepCount := stepCount^ + 1;

      currentDirections := currentDirections^ |> List.tail |> Option.getOrThrow;

      currentNode := node.left;
    | (Some(Right), Some(node)) =>
      stepCount := stepCount^ + 1;

      currentDirections := currentDirections^ |> List.tail |> Option.getOrThrow;

      currentNode := node.right;
    | (None, _) => currentDirections := directions
    | (Some(Left), None)
    | (Some(Right), None) =>
      errorState := Error("Couldn't find node with name: " ++ currentNode^)
    };

    if (currentNode^ == lastNodeName) {
      foundEnd := true;
    };

    if (stepCount^ > 100000) {
      errorState := Error("Too many steps");
      foundEnd := true;
    };
  };

  errorState^ |> Result.map(_ => stepCount^);
};

let parse =
  String.splitList(~delimiter="\n")
  >> List.uncons
  >> Result.fromOption("Failed to grab directions from first line")
  >> Result.flatMap(((directionsRaw, rest)) =>
       directionsRaw
       |> parseDirections
       |> Result.map(directions => (directions, rest))
     )
  >> Result.flatMap(((directions, rest)) =>
       rest
       |> List.uncons
       |> Result.fromOption("Failed to grab second line of input")
       |> Result.flatMap(((shouldBeEmpty, nodesRaw)) =>
            switch (shouldBeEmpty) {
            | "" => Ok((directions, nodesRaw))
            | _ => Error("Expected empty line between directions and nodes")
            }
          )
     )
  >> Result.flatMap(((directions, nodesRaw)) =>
       nodesRaw
       |> parseNodes
       |> Result.map(nodes =>
            {directions, nodes, firstNodeName: "AAA", lastNodeName: "ZZZ"}
          )
     );

let doPart1 =
  parse
  >> Result.flatMap(countStepsToEnd)
  >> Shared.Result.mapWithErrorText(steps => {j|Steps: $steps|j});
