type direction =
  | Left
  | Right;

[@ocaml.warning "-69"]
type node = {
  left: string,
  right: string,
};

[@ocaml.warning "-69"]
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
  |> Result.flatMap(nodePairs => {
       nodePairs
       |> List.head
       |> Result.fromOption("Failed to grab first node name")
       |> Result.flatMap(((firstNodeName, _)) =>
            nodePairs
            |> List.last
            |> Result.fromOption("Failed to grab last node name")
            |> Result.map(((lastNodeName, _)) =>
                 (firstNodeName, lastNodeName)
               )
          )
       |> Result.map(((firstNodeName, lastNodeName)) => {
            (firstNodeName, lastNodeName, nodePairs |> StringMap.fromList)
          })
     });
};

let countStepsToEnd =
    ({directions, nodes, firstNodeName, lastNodeName}: directionsAndNodes) => {
  let rec loop = (currentNodeName, steps) =>
    switch (currentNodeName) {
    | name when name == lastNodeName => Ok(steps)
    | name =>
      nodes
      |> StringMap.get(name)
      |> Result.fromOption("Failed to find node")
      |> Result.flatMap(node =>
           switch (directions |> List.head) {
           | Some(Left) => loop(node.left, steps + 1)
           | Some(Right) => loop(node.right, steps + 1)
           | None => Error("Ran out of directions")
           // | None => loop
           }
         )
    };
  loop(firstNodeName, 0);
};

let countStepsToEnd =
    ({directions, nodes, firstNodeName, lastNodeName}: directionsAndNodes) => {
  let stepCount = ref(0);
  let foundEnd = ref(false);

  while (! foundEnd^) {
    {
      // TODO: convert to a loop since there's too much recursion otherwise
    };
  };
};

let doPart1 =
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
       |> Result.map(((firstNodeName, lastNodeName, nodes)) =>
            {directions, nodes, firstNodeName, lastNodeName}
          )
     )
  >> Result.flatMap(countStepsToEnd)
  >> Js.Json.stringifyAny
  >> Option.getOrThrow;

let doPart2 = _ => "Not yet implemented";

let p1TestInput = Day08Data.testInput;

let p2TestInput = "Not there yet";

let actualInput = Day08Data.actualInput;

let doSandbox = None;

let sandboxInput = None;
