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

// let test = List.String.max(["1", "2", "3"]);
let parse =
  String.splitList(~delimiter="\n")
  >> splitList(~delimiter="")
  >> List.map(
       List.map(Int.fromString >> Option.getOrThrow) >> List.foldLeft((+), 0),
     );

let doWork = partFunc => parse >> partFunc >> Int.toString;

let part1 = List.maxBy(Int.compare) >> Option.getOrThrow;

let part2 =
  List.sortBy(Int.compare |> Ord.reverse) >> List.take(3) >> List.Int.sum;

module P = {
  [@react.component]
  let make = (~children) => <div> {children |> React.string} </div>;
};

[@react.component]
let make = () =>
  <div>
    <div>
      <h1 className="title"> {"Test data" |> React.string} </h1>
      <P> "input:" </P>
      <pre> {D01data.testInput |> React.string} </pre>
      <P> "output:" </P>
      <P> {D01data.testInput |> doWork(part1)} </P>
    </div>
    <div>
      <h1> {"Actual data (result only)" |> React.string} </h1>
      <P> {D01data.actualInput |> doWork(part1)} </P>
    </div>
  </div>;
// <pre> {testInput |> D01ml.doWork(D01ml.part1) |> React.string} </pre>;

// let make = () => <ul> <li> "1"->React.string </li> </ul>;
//  ->React.array}
//  ->Belt.List.toArray
//    )
//      </li>
//        {x->Belt.Int.toString->React.string}
//      <li key={i->Belt.Int.toString}>
//  ->Belt.List.mapWithIndex((i, x) =>
// {D01ml.testing123
