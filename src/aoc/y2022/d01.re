let testInput = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000";

let splitList = (~delimiter) =>
  List.foldLeft(
    (b, a) =>
      a == delimiter
        ? b |> List.append([])
        : b
          |> List.initOrEmpty
          |> List.append(
               b |> List.last |> Option.getOrElse([]) |> List.append(a),
             ),
    [[]],
  );

let test = List.String.max(["1", "2", "3"]);

[@react.component]
let make = () => <pre> testInput->React.string </pre>;
// let make = () => <ul> <li> "1"->React.string </li> </ul>;
//  ->React.array}
//  ->Belt.List.toArray
//    )
//      </li>
//        {x->Belt.Int.toString->React.string}
//      <li key={i->Belt.Int.toString}>
//  ->Belt.List.mapWithIndex((i, x) =>
// {D01ml.testing123
