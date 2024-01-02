type t;

type coord = {
  x: int,
  y: int,
};

let coordEq: (coord, coord) => bool;

let coordToString: coord => string;

type coordRange = {
  start_: coord,
  end_: coord,
};

let get: (coord, t) => option(string);

let fromStringBlock: string => t;

let findByValue: (string, t) => list(coord);

let getRange: (coordRange, t) => list(list(option(string)));

let getRangeWithCoords: (coordRange, t) => list((coord, option(string)));
