/*
   Strategy:
   * Find the coords of starting point ("S").
   * Find the coords of each first step of the loop (clockwise, counter-clockwise).
   * Follow the loop around the circle both directions till reaching
      start coords again.
   * For each path, track the step location as well as it's distance from start.
   * At the end, take the smallest value for each step location as it's actual
      distance (of the 2 possible values: clockwise and counter-clockwise).
   * Then, find the largest of those values.
 */

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

type edgeInfo = {
  previousCoord: Shared.Coord.t,
  currentCoord: Shared.Coord.t,
  stepCount: int,
};

let getEdges = (grid: Shared.Grid.t, coord: Shared.Coord.t) => {
  let coordsToEdgeInfo =
      (
        {x, y} as neighborCoord: Shared.Coord.t,
        (neighbor1: Shared.Coord.t, neighbor2: Shared.Coord.t),
      ) => {
    let neighbor1: Shared.Coord.t = {x: x + neighbor1.x, y: y + neighbor1.y};

    let neighbor2: Shared.Coord.t = {x: x + neighbor2.x, y: y + neighbor2.y};

    let coordString = coord |> Shared.Coord.toString;
    let neighborCoordString = neighborCoord |> Shared.Coord.toString;
    let neighbor1String = neighbor1 |> Shared.Coord.toString;
    let neighbor2String = neighbor2 |> Shared.Coord.toString;

    Js.log(
      {j|coord: $coordString,
neighborCoord: $neighborCoordString,
neighbor1: $neighbor1String,
neighbor2: $neighbor2String|j},
    );

    if (Shared.Coord.eq(coord, neighbor1)) {
      Js.log(
        {j|{
  previousCoord: $neighborCoordString,
  currentCoord: $neighbor2String
  stepCount: 1
}|j},
      );

      Ok(
        Some({
          previousCoord: coord,
          currentCoord: neighborCoord,
          stepCount: 1,
        }),
      );
    } else if (Shared.Coord.eq(coord, neighbor2)) {
      Js.log(
        {j|{
  previousCoord: $neighborCoordString,
  currentCoord: $neighbor1String
  stepCount: 1
}|j},
      );

      Ok(
        Some({
          previousCoord: coord,
          currentCoord: neighborCoord,
          stepCount: 1,
        }),
      );
    } else {
      Ok(None);
    };
  };

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
              |> coordsToEdgeInfo(neighborCoord)
            }
          )
     )
  |> List.Result.sequence
  |> Result.map(List.catOptions);
};

let walkEdge =
    (
      grid: Shared.Grid.t,
      startPoint: Shared.Coord.t,
      {previousCoord, currentCoord, stepCount} as edgeInfo: edgeInfo,
    )
    : result(list(edgeInfo), string) => {
  let nextCoord =
      (
        grid: Shared.Grid.t,
        {previousCoord, currentCoord: {x, y} as currentCoord, stepCount}: edgeInfo,
      ) =>
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

           if (Shared.Coord.eq(previousCoord, neighbor1)
               && !Shared.Coord.eq(neighbor2, startPoint)) {
             Ok(
               `next({
                 previousCoord: currentCoord,
                 currentCoord: neighbor2,
                 stepCount: stepCount + 1,
               }),
             );
           } else if (Shared.Coord.eq(previousCoord, neighbor2)
                      && !Shared.Coord.eq(neighbor1, startPoint)) {
             Ok(
               `next({
                 previousCoord: currentCoord,
                 currentCoord: neighbor1,
                 stepCount: stepCount + 1,
               }),
             );
           } else if (Shared.Coord.eq(neighbor1, startPoint)
                      || Shared.Coord.eq(neighbor2, startPoint)) {
             Ok(`finished);
           } else {
             let previousCoord = previousCoord |> Shared.Coord.toString;
             let neighbor1 = neighbor1 |> Shared.Coord.toString;
             let neighbor2 = neighbor2 |> Shared.Coord.toString;
             Error(
               {j|Couldn't find next coord! previous: $previousCoord, n1: $neighbor1, n2: $neighbor2|j},
             );
           };
         }
       );

  let rec loop = (grid, edgeInfo: edgeInfo, acc) =>
    switch (edgeInfo |> nextCoord(grid)) {
    | Ok(`finished) => Ok(List.reverse(acc))
    | Ok(`next(edgeInfo)) => loop(grid, edgeInfo, [edgeInfo, ...acc])
    | Error(err) => Error(err)
    };

  loop(
    grid,
    {previousCoord, currentCoord, stepCount},
    [
      edgeInfo,
      {
        previousCoord: {
          x: (-42),
          y: (-42),
        },
        currentCoord: startPoint,
        stepCount: 0,
      },
    ],
  );
};

let walkEdges =
    (
      grid: Shared.Grid.t,
      startPoint: result(Shared.Coord.t, string),
      (edge1, edge2),
    ) =>
  startPoint
  |> Result.flatMap(startPoint =>
       edge1
       |> walkEdge(grid, startPoint)
       |> Result.flatMap(edges1 =>
            edge2
            |> walkEdge(grid, startPoint)
            |> Result.map(edges2 => (edges1, edges2))
          )
     );

let verifySingleResult =
  fun
  | [coord] => Ok(coord)
  | _ => Error("Expected a single set of coordinates");

let verifyTwoEdges = (edges: list(edgeInfo)) =>
  switch (edges) {
  | [edge1, edge2] => Ok((edge1, edge2))
  | _ => Error({j|Expected 2 edges, got: $edges|j})
  };

let doPart1 = inputStr => {
  let grid = Shared.Grid.fromStringBlock(inputStr);

  let startPoint = grid |> Shared.Grid.findByValue("S") |> verifySingleResult;

  startPoint
  |> Result.flatMap(getEdges(grid))
  |> Result.flatMap(verifyTwoEdges)
  |> Result.flatMap(walkEdges(grid, startPoint))
  |> Result.tap(((edges1, edges2)) => {
       Js.log2("edges1: ", edges1 |> List.toArray);
       Js.log2("edges2: ", edges2 |> List.toArray);
     })
  |> Result.map(((edges1, edges2)) => {
       let edgeMap =
         edges1
         |> List.map(edge => (edge.currentCoord, edge.stepCount))
         |> Shared.Coord.Map.fromList;

       edges2
       |> List.foldLeft(
            (acc, edge: edgeInfo) =>
              acc
              |> Shared.Coord.Map.update(
                   edge.currentCoord, (stepCountOpt: option(int)) =>
                   stepCountOpt
                   |> (
                     fun
                     | Some(stepCount) =>
                       Some(Int.min(stepCount, edge.stepCount))
                     | None =>
                       failwith(
                         "Found a coord in edges2 that wasn't in edges1",
                       )
                   )
                 ),
            edgeMap,
          )
       |> Shared.Coord.Map.toList
       |> (
         lst => {
           Js.log2("min'd list: ", lst |> List.toArray);
           lst;
         }
       )
       |> List.maxBy(((_coord1, stepCount1), (_coord2, stepCount2)) =>
            Int.compare(stepCount1, stepCount2)
          )
       |> Option.map(Tuple.second);
     })
  |> Result.fold(
       err => {j|Error: $err|j},
       Option.fold("No result found", Int.toString),
     );
};

[@ocaml.warning "-69"]
type pipeInfo = {
  coord: Shared.Coord.t,
  pipeDirection,
};
/*
   (0, 1) = South
   (0, -1) = North
   (1, 0) = East
   (-1, 0) = West
 */

let getNeighborPipes = (grid, coord: Shared.Coord.t) =>
  coord
  |> getEdges(grid)
  |> Result.flatMap(verifyTwoEdges)
  |> Result.flatMap(((edge1, edge2)) =>
       switch (
         (edge1.currentCoord.x - coord.x, edge1.currentCoord.y - coord.y),
         (edge2.currentCoord.x - coord.x, edge2.currentCoord.y - coord.y),
       ) {
       | ((0, 1), (1, 0))
       | ((1, 0), (0, 1)) => Ok(SouthToEast)
       | (((-1), 0), (0, 1))
       | ((0, 1), ((-1), 0)) => Ok(SouthToWest)
       | ((1, 0), (0, (-1)))
       | ((0, (-1)), (1, 0)) => Ok(NorthToEast)
       | ((0, (-1)), ((-1), 0))
       | (((-1), 0), (0, (-1))) => Ok(NorthToWest)
       | ((0, (-1)), (0, 1))
       | ((0, 1), (0, (-1))) => Ok(NorthToSouth)
       | (((-1), 0), (1, 0))
       | ((1, 0), ((-1), 0)) => Ok(EastToWest)
       | ((x1, y1), (x2, y2)) =>
         Error(
           "Couldn't find pipe direction for coord: "
           ++ Shared.Coord.toString(coord)
           ++ ", edge1: "
           ++ Shared.Coord.toString(edge1.currentCoord)
           ++ ", edge2: "
           ++ Shared.Coord.toString(edge2.currentCoord)
           ++ {j|(x1: $x1, y1: $y1, x2: $x2, y2: $y2)|j},
         )
       }
     );

let edgeInfoToPipeInfo = (grid: Shared.Grid.t, edges: list(edgeInfo)) => {
  edges
  |> List.map(edge => {
       let coord = edge.currentCoord;

       grid
       |> Shared.Grid.get(coord)
       |> Result.fromOption("No value found at coord")
       |> Result.flatMap(parseTile)
       |> Result.flatMap(
            fun
            | Ground => Error("Ground tile found in edge")
            | Start =>
              coord
              |> getNeighborPipes(grid)
              |> Result.map(pipeDirection => {coord, pipeDirection})
            | Pipe(direction) => Ok({coord, pipeDirection: direction}),
          );
     })
  |> List.Result.sequence;
};

let isInside = (pipes: list(pipeInfo), coord: Shared.Coord.t) => {
  pipes
  |> List.filter(pipe => pipe.coord.x < coord.x && pipe.coord.y == coord.y)
  |> (
    x => {
      Js.log2("filtered x: ", x |> List.length);
      x;
    }
  )
  |> List.map(
       fun
       | {coord: _, pipeDirection: NorthToSouth} => 2
       | {coord: _, pipeDirection: EastToWest} => 0
       // Make opposing corners add up to 4
       //   while adjacent corners add up to 2 or 6
       //   this fixes the final problem with this method of testing
       | {coord: _, pipeDirection: NorthToWest | SouthToEast} => 1
       | {coord: _, pipeDirection: NorthToEast | SouthToWest} => 3,
     )
  |> List.Int.sum
  |> (
    x => {
      let coordStr = coord |> Shared.Coord.toString;
      Js.log({j|coord: $coordStr -- sum: $x|j});
      x;
    }
  )
  |> (
    x => {
      let res = Int.modulo(x, 4) != 0;

      if (res) {
        let coordStr = coord |> Shared.Coord.toString;
        Js.log({j|coord: $coordStr -- x: $x|j});
      };

      res;
    }
  )
  |> (
    x => {
      let coordStr = coord |> Shared.Coord.toString;
      if (x) {
        Js.log({j|coord: $coordStr -- isInside: $x|j});
      };
      x;
    }
  );
};

let doPart2 = inputStr => {
  let grid = Shared.Grid.fromStringBlock(inputStr);

  let startPoint = grid |> Shared.Grid.findByValue("S") |> verifySingleResult;

  let (>>=) = (a, b) => a |> Result.flatMap(b);

  startPoint
  >>= getEdges(grid)
  >>= verifyTwoEdges
  >>= walkEdges(grid, startPoint)
  >>= (Tuple.first >> edgeInfoToPipeInfo(grid))
  |> Result.map(pipeInfos =>
       grid
       |> Shared.Grid.getAllCoords
       |> (
         x => {
           Js.log2("all coords: ", x |> Array.length);
           x;
         }
       )
       |> Array.filter(coord =>
            !
              List.containsBy(
                Shared.Coord.eq,
                coord,
                pipeInfos |> List.map(pipeInfo => pipeInfo.coord),
              )
          )
       |> (
         x => {
           Js.log2("filtered coords 1: ", x |> Array.length);
           x;
         }
       )
       |> Array.filter(coord => isInside(pipeInfos, coord))
       |> (
         x => {
           Js.log2("filtered coords 2: ", x |> Array.length);
           Js.log2("filtered coords 2: ", x);
           x;
         }
       )
       |> Array.length
     )
  |> Result.fold(err => {j|Error: $err|j}, Int.toString);
};

let p1TestInput = Day10Data.testInput1;

let p2TestInput = Day10Data.p2TestInput1;

let actualInput = Day10Data.actualInput;
// let actualInput = "Temporarily removed";

let doSandbox = None;

let sandboxInput = None;
