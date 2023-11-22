module P = {
  [@react.component]
  let make = (~children) => <div> {children |> React.string} </div>;
};

[@react.component]
let make = (~dayInfo: (module Shared.DayInfo.DayInfo)) => {
  let (module Day) = dayInfo;

  let (input, setInput) = React.useState(() => Day.testInput);

  let (part1Result, setPart1Result) =
    React.useState(() => Day.testInput |> Day.doPart1);

  let (part2Result, setPart2Result) =
    React.useState(() => Day.testInput |> Day.doPart2);

  let setInput = i => {
    setInput(_ => i);
    setPart1Result(_ => i |> Day.doPart1);
    setPart2Result(_ => i |> Day.doPart2);
  };

  <div className="container mt-6 is-centered">
    <div className="columns is-centered">
      <div className="column is-three-quarters">
        <a href="#" onClick={_ => ReasonReactRouter.push("/")}>
          {"<- Home" |> React.string}
        </a>
      </div>
    </div>
    <div className="columns is-centered">
      <div className="column is-half">
        <div className="box">
          <div className="columns">
            <div className="column">
              <h1 className="title"> {"Input data" |> React.string} </h1>
            </div>
            <div className="column">
              <button
                className="button is-pulled-right is-info"
                onClick={_ => setInput(Day.testInput)}>
                {"Load test data" |> React.string}
              </button>
            </div>
            <div className="column">
              <button
                className="button is-pulled-right is-info"
                onClick={_ => setInput(Day.actualInput)}>
                {"Load actual data" |> React.string}
              </button>
            </div>
          </div>
          <textarea
            className="textarea"
            rows=20
            value=input
            onChange={e => setInput(ReactEvent.Form.target(e)##value)}
          />
        </div>
      </div>
      <div className="column is-one-quarter">
        <div className="box">
          <h1 className="title"> {"Results" |> React.string} </h1>
          <br />
          <h2 className="subtitle"> {"Part 1" |> React.string} </h2>
          <textarea className="textarea" value=part1Result readOnly=true />
          <br />
          <h2 className="subtitle"> {"Part 2" |> React.string} </h2>
          <textarea className="textarea" value=part2Result readOnly=true />
        </div>
      </div>
    </div>
  </div>;
};
