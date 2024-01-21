type t = {
  c1: Shared__Coord.t,
  c2: Shared__Coord.t,
};

let eq = ({c1: c1a, c2: c2a}, {c1: c1b, c2: c2b}) =>
  Shared__Coord.eq(c1a, c1b)
  && Shared__Coord.eq(c2a, c2b)
  || Shared__Coord.eq(c1a, c2b)
  && Shared__Coord.eq(c2a, c1b);

let toString = ({c1: {x: x1, y: y1}, c2: {x: x2, y: y2}}) =>
  "CoordPair: ("
  ++ Int.toString(x1)
  ++ ", "
  ++ Int.toString(y1)
  ++ "), ("
  ++ Int.toString(x2)
  ++ ", "
  ++ Int.toString(y2)
  ++ ")";

module Ord = {
  type nonrec t = t;

  let eq = eq;

  /*
     The lt/gt logic in this compare is problematic.
     It mostly just considers direction along the y=x line in
       the positive direction to be the decider of magnitude.
     While not ideal, it should be fine for my only current
       planned use: as a key in a map.
   */
  let compare = ({c1: c1a, c2: c2a}, {c1: c1b, c2: c2b}) =>
    if (Shared__Coord.eq(c1a, c1b)) {
      if (Shared__Coord.eq(c2a, c2b)) {
        `equal_to;
      } else if (c2a.x < c2b.x || c2a.y < c2b.y) {
        `less_than;
      } else {
        `greater_than;
      };
    } else if (c1a.x < c1b.x || c1a.y < c1b.y) {
      `less_than;
    } else {
      `greater_than;
    };
};

module Map = {
  include Map.WithOrd(Ord);
};
