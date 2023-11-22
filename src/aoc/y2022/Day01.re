module Day01: Shared.DayInfo.DayInfo = {
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

  let parse =
    String.splitList(~delimiter="\n")
    >> splitList(~delimiter="")
    >> List.map(
         List.map(Int.fromString >> Option.getOrThrow)
         >> List.foldLeft((+), 0),
       );

  let doWork = partFunc => parse >> partFunc >> Int.toString;

  let part1 = List.maxBy(Int.compare) >> Option.getOrThrow;

  let part2 =
    List.sortBy(Int.compare |> Ord.reverse) >> List.take(3) >> List.Int.sum;

  // type t = {

  let doPart1 = doWork(part1);

  let doPart2 = doWork(part2);

  let testInput = Day01Data.testInput;

  let actualInput = Day01Data.actualInput;
  // module P = {
  //   [@react.component]
  //   let make = (~children) => <div> {children |> React.string} </div>;
  // };
  // [@react.component]
  // let make = () => {
  //   let (input, setInput) = React.useState(() => Day01Data.testInput);
  //   let (result, setResult) =
  //     React.useState(() => Day01Data.testInput |> doWork(part1));
  //   let setInput = i => {
  //     setInput(_ => i);
  //     setResult(_ => i |> doWork(part1));
  //   };
  //   <div className="container mt-6">
  //     <div className="columns is-centered">
  //       <div className="column is-half">
  //         <div className="box">
  //           <div className="columns">
  //             <div className="column">
  //               <h1 className="title"> {"Input data" |> React.string} </h1>
  //             </div>
  //             <div className="column">
  //               <button
  //                 className="button is-pulled-right is-info"
  //                 onClick={_ => setInput(Day01Data.testInput)}>
  //                 {"Load test data" |> React.string}
  //               </button>
  //             </div>
  //             <div className="column">
  //               <button
  //                 className="button is-pulled-right is-info"
  //                 onClick={_ => setInput(Day01Data.actualInput)}>
  //                 {"Load actual data" |> React.string}
  //               </button>
  //             </div>
  //           </div>
  //           <textarea
  //             className="textarea"
  //             rows=20
  //             value=input
  //             onChange={e => setInput(ReactEvent.Form.target(e)##value)}
  //           />
  //         </div>
  //       </div>
  //       <div className="column is-half">
  //         <div className="box">
  //           <div className="columns">
  //             <div className="column">
  //               <h1 className="title"> {"Result" |> React.string} </h1>
  //             </div>
  //           </div>
  //           <textarea className="textarea" rows=20 value=result />
  //         </div>
  //       </div>
  //     </div>
  //   </div>;
  // };
};
