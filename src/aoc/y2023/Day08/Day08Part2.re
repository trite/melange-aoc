type direction =
  | Left
  | Right;

// [@ocaml.warning "-69"]
type node = {
  left: string,
  right: string,
};

// [@ocaml.warning "-69"]
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
    ({directions, nodes, currentNodeNames}: directionsAndNodes)
    : result(int, string) => {
  let stepCount = ref(0);
  let foundEnd = ref(false);
  let currentNodeNames = ref(currentNodeNames);
  let errorState: ref(result(unit, string)) = ref(Ok());
  let currentDirections = ref(directions);

  // let getNode = (nodeName: string) => nodes |> StringMap.get(nodeName);

  while (! foundEnd^ && errorState^ == Ok()) {
    // If we've run out of directions, restart the list
    if (currentDirections^ == []) {
      currentDirections := directions;
    };

    stepCount := stepCount^ + 1;

    currentNodeNames^
    |> List.map(currentNode => {
         switch (
           currentDirections^ |> List.head,
           nodes |> StringMap.get(currentNode),
         ) {
         | (Some(Left), Some(node)) => Ok(node.left)
         | (Some(Right), Some(node)) => Ok(node.right)
         | (None, Some(_)) =>
           // shouldn't be possible
           Error("Shouldn't have an empty set of directions here!")
         | (_, None) =>
           // we've hit a dead end
           Error("Couldn't find node with name: " ++ currentNode)
         }
       })
    |> List.Result.sequence
    |> Result.fold(
         // This is a horrible way to do this, but leaving it for now
         err => {
           errorState := Error(err);
           foundEnd := true;
           ();
         },
         newCurrentNodeNames => {
           currentDirections :=
             currentDirections^ |> List.tail |> Option.getOrThrow;
           currentNodeNames := newCurrentNodeNames;
           ();
         },
       );
    // |> Result.map(_newCurrentNodeNames => {
    //      // This is a horrible way to do this, but leaving it for now
    //      currentDirections :=
    //        currentDirections^ |> List.tail |> Option.getOrThrow;
    //      ();
    //      //  newCurrentNodeNames;
    //    })
    // |> Result.mapError(err => {
    //      errorState := Error(err);
    //      foundEnd := true;
    //      ();
    //    })
    // |> Result.fold(
    //      newCurrentNodeNames => {currentNodeNames := newCurrentNodeNames},
    //      error => {errorState := Error(error)},
    //    );

    // replace with a fold over currentNode
    // if (currentNode^ == lastNodeName) {
    //   foundEnd := true;
    // };

    currentNodeNames^
    |> List.map(currentNodeName =>
         currentNodeName |> String.endsWith(~search="Z")
       )
    |> List.foldLeft((acc, x) => acc && x, true)
    |> (
      allEndNodes =>
        if (allEndNodes) {
          foundEnd := true;
        }
    );

    if (stepCount^ > 1_000_000_000) {
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
  >> Result.flatMap(((directions, nodesRaw)) => {
       nodesRaw
       |> parseNodes
       |> Result.map(((nodeNames, nodes)) => {
            {
              // Js.log("nodes raw: " ++ (nodesRaw |> List.String.joinWith(",")));

              directions,
              nodes,
              currentNodeNames:
                nodeNames |> List.filter(String.endsWith(~search="A")),
            }
          })
     });

// TODO: this version should eventually work, but may take a very long time to finish
let doPart2 =
  parse
  >> Result.tap(_ => Js.log("About to start counting steps - part 2"))
  // >> Result.tap(({currentNodeNames, _})
  //      //  Js.log(
  //      //    "directions: " ++
  //      //    directions ++
  //      //    "nodes: " ++
  //      //    nodes ++
  //      //    "currentNodeNames: " ++
  //      //    currentNodeNames,
  //      //  )
  //      =>
  //        Js.log(
  //          "starting Nodes: "
  //          ++ (currentNodeNames |> List.String.joinWith(",")),
  //        )
  //      )
  >> Result.flatMap(countStepsToEnd)
  >> Result.tap(_ => Js.log("Finished counting steps - part 2"))
  >> Result.fold(err => {j|Error: $err|j}, steps => {j|Steps: $steps|j});
