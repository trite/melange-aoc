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

let pipeDirectionToCoords:
  pipeDirection => (Shared.Grid.coord, Shared.Grid.coord) = {
  open Shared.Grid;
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

let getNeighbors = ({x, y}: Shared.Grid.coord, grid: Shared.Grid.t) =>
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
  previousCoord: Shared.Grid.coord,
  currentCoord: Shared.Grid.coord,
  stepCount: int,
};

let getEdges = (grid: Shared.Grid.t, coord: Shared.Grid.coord) =>
  grid
  |> getNeighbors(coord)
  |> List.map(lst => {
       Js.log2("lst: ", lst);
       lst;
     })
  |> List.map(
       (
         (
           {x, y} as neighborCoord: Shared.Grid.coord,
           valOpt: option(string),
         ),
       ) =>
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
                  let neighbor1: Shared.Grid.coord = {
                    x: x + neighbor1.x,
                    y: y + neighbor1.y,
                  };

                  let neighbor2: Shared.Grid.coord = {
                    x: x + neighbor2.x,
                    y: y + neighbor2.y,
                  };

                  let coordString = neighborCoord |> Shared.Grid.coordToString;
                  let neighbor1String = neighbor1 |> Shared.Grid.coordToString;
                  let neighbor2String = neighbor2 |> Shared.Grid.coordToString;

                  Js.log(
                    {j|coord: $coordString, neighbor1: $neighbor1String, neighbor2: $neighbor2String|j},
                  );

                  if (Shared.Grid.coordEq(coord, neighbor1)) {
                    // Js.log("NEIGHBOR 1");
                    // Ok(Some((2, neighbor2)));
                    Ok(
                      Some({
                        previousCoord: neighborCoord,
                        currentCoord: neighbor2,
                        stepCount: 2,
                      }),
                    );
                  } else if (Shared.Grid.coordEq(coord, neighbor2)) {
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
    (
      grid: Shared.Grid.t,
      {previousCoord, currentCoord: {x, y} as currentCoord, stepCount}: edgeInfo,
      visited: list(edgeInfo),
    ) => {
  let nextCoord =
    Shared.Grid.get(currentCoord, grid)
    |> Result.fromOption("No value found at coord")
    |> Result.flatMap(parseTile)
    |> Result.flatMap(tile =>
         switch (tile) {
         | Ground => Error("Ground tile found in edge")
         | Start =>
           Error(
             "Start tile found in edge (this will eventually be a success condition)",
           )
         | Pipe(direction) => Ok(pipeDirectionToCoords(direction))
         }
       )
    |> Result.flatMap(
         ((neighbor1: Shared.Grid.coord, neighbor2: Shared.Grid.coord)) => {
         let neighbor1: Shared.Grid.coord = {
           x: x + neighbor1.x,
           y: y + neighbor1.y,
         };

         let neighbor2: Shared.Grid.coord = {
           x: x + neighbor2.x,
           y: y + neighbor2.y,
         };

         if (Shared.Grid.coordEq(previousCoord, neighbor1)) {
           Ok({
             previousCoord: currentCoord,
             currentCoord: neighbor2,
             stepCount: 2,
           });
         } else if (Shared.Grid.coordEq(previousCoord, neighbor2)) {
           Ok({
             previousCoord: currentCoord,
             currentCoord: neighbor1,
             stepCount: 2,
           });
         } else {
           Error("Couldn't find next coord!");
         };
       })
    |> Result.map()
                 // TODO: resume here
;
  ();
};

let doPart1 = inputStr => {
  let grid = Shared.Grid.fromStringBlock(inputStr);

  grid
  |> Shared.Grid.findByValue("S")
  |> verifySingleResult
  |> Result.flatMap(getEdges(grid))
  |> Result.flatMap(verifyTwoEdges)
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
