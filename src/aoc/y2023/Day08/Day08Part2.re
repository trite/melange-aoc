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
  currentNodeNames: list(string),
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
  |> Result.map(lst =>
       (lst |> List.map(Tuple2.first), StringMap.fromList(lst))
     );
};

let countStepsToEnd =
    ({directions, nodes, _}: directionsAndNodes, firstNodeName) => {
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

    if (currentNode^ |> String.endsWith(~search="Z")) {
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
       |> Result.map(((nodeNames, nodes)) =>
            {
              directions,
              nodes,
              currentNodeNames:
                nodeNames |> List.filter(String.endsWith(~search="A")),
            }
          )
     );

// Fails because of int overflow
let _lcm = (m, n) => {
  let rec gcd = (u, v) =>
    if (v != 0) {
      gcd(v, Int.modulo(u, v));
    } else {
      Int.abs(u);
    };

  switch (m, n) {
  | (0, _)
  | (_, 0) => 0
  | (m, n) => Int.abs(m * n) / gcd(m, n)
  };
};

// Returns infinity
let _jsLcm: (float, float) => float = [%raw
  {j|
  function leastCommonMultiple(min, max) {
      function range(min, max) {
          var arr = [];
          for (var i = min; i <= max; i++) {
              arr.push(i);
          }
          return arr;
      }

      function gcd(a, b) {
          return !b ? a : gcd(b, a % b);
      }

      function lcm(a, b) {
          return (a * b) / gcd(a, b);
      }

      var multiple = min;
      range(min, max).forEach(function(n) {
          multiple = lcm(multiple, n);
      });

      return multiple;
  }
|j}
];

// TODO: This will just print the cycles of each node
//         Answer is least common multiple of these values
let doPart2 =
  parse
  >> Result.tap(_ => Js.log("About to start counting steps - part 2"))
  >> Result.flatMap(({currentNodeNames, _} as directionsAndNodes) =>
       currentNodeNames
       |> List.map(currentNodeName =>
            (
              currentNodeName,
              countStepsToEnd(directionsAndNodes, currentNodeName),
            )
          )
       |> List.foldLeft(
            (acc, (nodeName, result)) =>
              acc
              |> Result.flatMap(lst =>
                   result |> Result.map(steps => [(nodeName, steps), ...lst])
                 ),
            Ok([]),
          )
     )
  >> Result.tap(_ => Js.log("Finished counting steps - part 2"))
  // >> Result.map(
  //      List.map(Tuple.second >> Int.toFloat) >> List.foldLeft(jsLcm, 1.),
  //    )
  // >> Result.fold(err => {j|Error: $err|j}, lcm => {j|LCM of these is $lcm|j});
  >> Result.fold(
       err => {j|Error: $err|j},
       List.map(((_nodeName, steps)) => {j|$steps|j})
       >> List.String.joinWith("\n")
       >> (++)("Result is LCM of these values:\n"),
     );

/*
 Results:
   15517
   19199
   20777
   11309
   17621
   13939

 LCM of these is 13663968099527

 TODO: write out something to calculate the LCM
 */
