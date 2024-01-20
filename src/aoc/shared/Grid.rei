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

let getRowAtY: (t, int) => option(list((Coord.t, string)));

let getRowAtYValues: (t, int) => option(list(string));

let getColAtX: (t, int) => option(list((Coord.t, string)));

let getColAtYValues: (t, int) => option(list(string));
