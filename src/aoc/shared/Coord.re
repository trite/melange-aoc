type t = {
  x: int,
  y: int,
};

let eq = ({x: x1, y: y1}, {x: x2, y: y2}) => x1 == x2 && y1 == y2;

let toString = ({x, y}) =>
  "(" ++ Int.toString(x) ++ ", " ++ Int.toString(y) ++ ")";

module Ord = {
  type nonrec t = t;

  let eq = eq;

  let compare = ({x: x1, y: y1}, {x: x2, y: y2}): Bastet.Interface.ordering =>
    if (x1 == x2) {
      if (y1 == y2) {
        `equal_to;
      } else if (y1 < y2) {
        `less_than;
      } else {
        `greater_than;
      };
    } else if (x1 < x2) {
      `less_than;
    } else {
      `greater_than;
    };
};

module Map = {
  include Map.WithOrd(Ord);
};
