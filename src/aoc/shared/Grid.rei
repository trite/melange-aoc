type t;

type coordRange = {
  start_: Coord.t,
  end_: Coord.t,
};

let get: (Coord.t, t) => option(string);

let fromStringBlock: string => t;

let findByValue: (string, t) => list(Coord.t);

let getRange: (coordRange, t) => list(list(option(string)));

let getRangeWithCoords: (coordRange, t) => list((Coord.t, option(string)));

let getAllCoords: t => array(Coord.t);
