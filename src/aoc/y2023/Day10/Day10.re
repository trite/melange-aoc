/*
   Perhaps try:
   Find the coords of starting point ("S").
   Find the coords of each first step of the loop (clockwise, counter-clockwise).
   Follow the loop around the circle both directions till reaching
     start coords again.
   For each path, track the step location as well as it's distance from start.
   At the end, take the smallest value for each step location as it's actual
     distance (of the 2 possible values: clockwise and counter-clockwise).
   Then, find the largest of those values.
 */

// let doPart1 = const("Part 1 not implemented yet");
// let doPart1 =
//   Shared.Grid.fromStringBlock
//   >> Shared.Grid.get(~x=1, ~y=1)
//   >> Option.getOrElse("FAILURE");

// let verifySingleResult
let verifySingleResult =
  fun
  | [coord] => Ok(coord)
  | _ => Error("Expected a single set of coordinates");

type pipeDirection =
  | NorthToSouth
  | EastToWest
  | NorthToEast
  | NorthToWest
  | SouthToEast
  | SouthToWest;

let pipeDirectionToString =
  fun
  | NorthToSouth => "NorthToSouth"
  | EastToWest => "EastToWest"
  | NorthToEast => "NorthToEast"
  | NorthToWest => "NorthToWest"
  | SouthToEast => "SouthToEast"
  | SouthToWest => "SouthToWest";

type tile =
  | Ground
  | Start
  | Pipe(pipeDirection);

let tileToString =
  fun
  | Ground => "Ground"
  | Start => "Start"
  | Pipe(direction) =>
    "Pipe(direction: " ++ pipeDirectionToString(direction) ++ ")";

let parseTile =
  fun
  | "." => Ok(Ground)
  | "|" => Ok(Pipe(NorthToSouth))
  | "-" => Ok(Pipe(EastToWest))
  | "L" => Ok(Pipe(NorthToEast))
  | "J" => Ok(Pipe(NorthToWest))
  | "7" => Ok(Pipe(SouthToWest))
  | "F" => Ok(Pipe(SouthToEast))
  | "S" => Ok(Start)
  | tile => Error({j|Invalid tile: "$tile"|j});

let pipeDirectionToCoords: pipeDirection => (Shared.Coord.t, Shared.Coord.t) = {
  open Shared.Coord;
  let north = {x: 0, y: (-1)};
  let south = {x: 0, y: 1};
  let east = {x: 1, y: 0};
  let west = {x: (-1), y: 0};

  fun
  | NorthToSouth => (north, south)
  | EastToWest => (east, west)
  | NorthToEast => (north, east)
  | NorthToWest => (north, west)
  | SouthToEast => (south, east)
  | SouthToWest => (south, west);
};

let getNeighbors = ({x, y}: Shared.Coord.t, grid: Shared.Grid.t) =>
  Shared.Grid.getRangeWithCoords(
    {
      start_: {
        x: x - 1,
        y: y - 1,
      },
      end_: {
        x: x + 1,
        y: y + 1,
      },
    },
    grid,
  )
  |> List.removeAt(4); // remove the center coord

[@ocaml.warning "-69"]
type edgeInfo = {
  previousCoord: Shared.Coord.t,
  currentCoord: Shared.Coord.t,
  stepCount: int,
};

let getEdges = (grid: Shared.Grid.t, coord: Shared.Coord.t) =>
  grid
  |> getNeighbors(coord)
  |> List.map(lst => {
       Js.log2("lst: ", lst);
       lst;
     })
  |> List.map(
       (({x, y} as neighborCoord: Shared.Coord.t, valOpt: option(string))) =>
       valOpt
       |> Result.fromOption({j|No value found at coord: (x:$x, y:$y)|j})
       |> Result.tap(Js.log2("valOpt: "))
       |> Result.flatMap(parseTile)
       |> Result.tap(tileToString >> Js.log2("tile: "))
       |> Result.flatMap(tile =>
            switch (tile) {
            | Ground => Ok(None)
            | Start =>
              Error({j|Start tile found in neighbors at coord ($x, $y)|j})
            | Pipe(direction) =>
              direction
              |> pipeDirectionToCoords
              |> (
                ((neighbor1, neighbor2)) => {
                  let neighbor1: Shared.Coord.t = {
                    x: x + neighbor1.x,
                    y: y + neighbor1.y,
                  };

                  let neighbor2: Shared.Coord.t = {
                    x: x + neighbor2.x,
                    y: y + neighbor2.y,
                  };

                  let coordString = neighborCoord |> Shared.Coord.toString;
                  let neighbor1String = neighbor1 |> Shared.Coord.toString;
                  let neighbor2String = neighbor2 |> Shared.Coord.toString;

                  Js.log(
                    {j|coord: $coordString, neighbor1: $neighbor1String, neighbor2: $neighbor2String|j},
                  );

                  if (Shared.Coord.eq(coord, neighbor1)) {
                    // Js.log("NEIGHBOR 1");
                    // Ok(Some((2, neighbor2)));
                    Ok(
                      Some({
                        previousCoord: neighborCoord,
                        currentCoord: neighbor2,
                        stepCount: 2,
                      }),
                    );
                  } else if (Shared.Coord.eq(coord, neighbor2)) {
                    // Js.log("NEIGHBOR 2");
                    // Ok(Some((2, neighbor1)));
                    Ok(
                      Some({
                        previousCoord: neighborCoord,
                        currentCoord: neighbor1,
                        stepCount: 2,
                      }),
                    );
                  } else {
                    // Js.log("NO NEIGHBORS");
                    Ok(None);
                  };
                }
              )
            }
          )
     )
  |> List.Result.sequence
  |> Result.map(List.catOptions);

let verifyTwoEdges = (edges: list(edgeInfo)) =>
  switch (edges) {
  | [edge1, edge2] => Ok((edge1, edge2))
  | _ => Error({j|Expected 2 edges, got: $edges|j})
  };

let walkEdge =
    (grid: Shared.Grid.t, {previousCoord, currentCoord, stepCount}: edgeInfo)
    : result(list(edgeInfo), string) => {
  let nextCoord =
      (grid: Shared.Grid.t, {x, y} as currentCoord: Shared.Coord.t) =>
    grid
    |> Shared.Grid.get(currentCoord)
    |> Result.fromOption("No value found at coord")
    |> Result.flatMap(parseTile)
    |> Result.flatMap(tile =>
         switch (tile) {
         | Ground => Error("Ground tile found in edge")
         | Start => Ok(`reachedStart)
         | Pipe(direction) =>
           Ok(`continue(pipeDirectionToCoords(direction)))
         }
       )
    |> Result.flatMap(
         (
           blah: [>
             | `continue(Shared.Coord.t, Shared.Coord.t)
             | `reachedStart
           ],
         ) =>
         switch (blah) {
         | `reachedStart => Ok(`finished)
         | `continue(neighbor1, neighbor2) =>
           let neighbor1: Shared.Coord.t = {
             x: x + neighbor1.x,
             y: y + neighbor1.y,
           };

           let neighbor2: Shared.Coord.t = {
             x: x + neighbor2.x,
             y: y + neighbor2.y,
           };

           if (Shared.Coord.eq(previousCoord, neighbor1)) {
             Ok(
               `next({
                 previousCoord: currentCoord,
                 currentCoord: neighbor2,
                 stepCount: stepCount + 1,
               }),
             );
           } else if (Shared.Coord.eq(previousCoord, neighbor2)) {
             Ok(
               `next({
                 previousCoord: currentCoord,
                 currentCoord: neighbor1,
                 stepCount: stepCount + 1,
               }),
             );
           } else {
             let previousCoord = previousCoord |> Shared.Coord.toString;
             let neighbor1 = neighbor1 |> Shared.Coord.toString;
             let neighbor2 = neighbor2 |> Shared.Coord.toString;
             //  Error("Couldn't find next coord!");
             Error(
               {j|Couldn't find next coord! previous: $previousCoord, n1: $neighbor1, n2: $neighbor2|j},
             );
           };
         }
       );

  let rec loop = (grid, edgeInfo: edgeInfo, acc) =>
    switch (edgeInfo.currentCoord |> nextCoord(grid)) {
    | Ok(`finished) => Ok(List.reverse(acc))
    | Ok(`next(edgeInfo)) => loop(grid, edgeInfo, [edgeInfo, ...acc])
    | Error(err) => Error(err)
    };

  loop(grid, {previousCoord, currentCoord, stepCount}, []);
};

let doPart1 = inputStr => {
  let grid = Shared.Grid.fromStringBlock(inputStr);

  grid
  |> Shared.Grid.findByValue("S")
  |> verifySingleResult
  |> Result.flatMap(getEdges(grid))
  |> Result.flatMap(verifyTwoEdges)
  |> Result.flatMap(((edge1, edge2)) =>
       edge1
       |> walkEdge(grid)
       |> Result.flatMap(edges1 =>
            edge2 |> walkEdge(grid) |> Result.map(edges2 => (edges1, edges2))
          )
     )
  |> Result.map(((edges1, edges2)) => {
       let edgeMap =
         edges1
         |> List.map(edge => (edge.currentCoord, edge.stepCount))
         |> Shared.Coord.Map.fromList;

       edges2
       |> List.foldLeft(
            // (acc, edge: edgeInfo) =>
            //   switch (Shared.Coord.Map.get(edge.currentCoord, edgeMap)) {
            //   | None =>
            //     failwith("Found a coord in edges2 that wasn't in edges1")
            //   | Some(stepCount) =>
            //     if (stepCount < edge.stepCount) {
            //       acc;
            //     } else {
            //       edgeMap
            //       |> Shared.Coord.Map.set(edge.currentCoord, edge.stepCount);
            //     }
            //   },
            (acc, edge: edgeInfo) =>
              acc
              |> Shared.Coord.Map.update(
                   edge.currentCoord, (stepCountOpt: option(int)) =>
                   stepCountOpt
                   |> (
                     fun
                     | Some(stepCount) =>
                       Some(
                         if (stepCount < edge.stepCount) {
                           stepCount;
                         } else {
                           edge.stepCount;
                         },
                       )
                     | None =>
                       failwith(
                         "Found a coord in edges2 that wasn't in edges1",
                       )
                   )
                 ),
            edgeMap,
          )
       |> Shared.Coord.Map.toList
       |> List.maxBy(((_coord1, stepCount1), (_coord2, stepCount2)) =>
            Int.compare(stepCount1, stepCount2)
          );
       //  |> List.maxBy(Shared.Coord.Ord.compare);
     })
  |> Result.fold(
       err => {j|Error: $err|j},
       Js.Json.stringifyAny >> Option.getOrThrow,
     );
};
// >> Result.fold(
//      err => {j|Error: $err|j},
//      Js.Json.stringifyAny >> Option.getOrThrow,
//    );

let doPart2 = const("Part 2 not implemented yet");

let p1TestInput = Day10Data.testInput1;

let p2TestInput = "Not there yet";

let actualInput = Day10Data.actualInput;

let doSandbox = None;

let sandboxInput = None;
