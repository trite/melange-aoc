type t;

type coordRange = {
  start_: Shared__Coord.t,
  end_: Shared__Coord.t,
};

let get: (Shared__Coord.t, t) => option(string);

let getHeight: t => int;

let getWidth: t => int;

let getRowsWithId: t => list((int, string));

let getRows: t => list(string);

let getColsWithId: t => list((int, string));

let getCols: t => list(string);

let transposeGrid: t => t;

let fromStringBlock: string => t;

let findByValue: (string, t) => list(Shared__Coord.t);

let getRange: (coordRange, t) => list(list(option(string)));

let getRangeWithCoords:
  (coordRange, t) => list((Shared__Coord.t, option(string)));

let getAllCoords: t => array(Shared__Coord.t);

let getRowAtY: (int, t) => option(list((Shared__Coord.t, string)));

let getRowAtYValues: (int, t) => option(list(string));

let getColAtX: (int, t) => option(list((Shared__Coord.t, string)));

let getColAtYValues: (int, t) => option(list(string));

let insertRowAt: (int, list(string), t) => result(t, string);

let insertColAt: (int, list(string), t) => result(t, string);

let toString: t => string;
