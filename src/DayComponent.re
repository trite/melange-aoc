module P = {
  [@react.component]
  let make = (~children) => <div> {children |> React.string} </div>;
};

[@react.component]
let make = (~dayInfo: (module Shared.DayInfo.DayInfo)) => {
  let (module Day) = dayInfo;

  let (part1TestInput, setPart1TestInput) =
    React.useState(() => Day.p1TestInput);

  let (part2TestInput, setPart2TestInput) =
    React.useState(() => Day.p2TestInput);

  let (actualInput, setActualInput) = React.useState(() => Day.actualInput);

  let (part1TestResult, setPart1TestResult) =
    React.useState(() => Day.p1TestInput |> Day.doPart1);

  let (part2TestResult, setPart2TestResult) =
    React.useState(() => Day.p2TestInput |> Day.doPart2);

  let (part1ActualResult, setPart1ActualResult) =
    React.useState(() => Day.actualInput |> Day.doPart1);

  let (part2ActualResult, setPart2ActualResult) =
    React.useState(() => Day.actualInput |> Day.doPart2);

  let setPart1TestInput = input => {
    setPart1TestInput(_ => input);
    setPart1TestResult(_ => input |> Day.doPart1);
  };

  let setPart2TestInput = input => {
    setPart2TestInput(_ => input);
    setPart2TestResult(_ => input |> Day.doPart2);
  };

  let setActualInput = input => {
    setActualInput(_ => input);
    setPart1ActualResult(_ => input |> Day.doPart1);
    setPart2ActualResult(_ => input |> Day.doPart2);
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
          </div>
          // <textarea
          //   className="textarea"
          //   rows=20
          //   value=input
          //   onChange={e => setInput(ReactEvent.Form.target(e)##value)}
          // />
          <br />
          <div className="columns">
            <div className="column">
              <h2 className="subtitle"> {"Part 1" |> React.string} </h2>
              <textarea
                className="textarea"
                value=part1TestInput
                onChange={e =>
                  setPart1TestInput(ReactEvent.Form.target(e)##value)
                }
              />
            </div>
            <br />
            <div className="column">
              <h2 className="subtitle"> {"Part 2" |> React.string} </h2>
              <textarea
                className="textarea"
                value=part2TestInput
                onChange={e =>
                  setPart2TestInput(ReactEvent.Form.target(e)##value)
                }
              />
            </div>
          </div>
          <h2 className="subtitle"> {"Actual" |> React.string} </h2>
          <textarea
            className="textarea"
            value=actualInput
            onChange={e => setActualInput(ReactEvent.Form.target(e)##value)}
          />
        </div>
      </div>
      <div className="column is-one-quarter">
        <div className="box">
          <h1 className="title"> {"Test Results" |> React.string} </h1>
          <br />
          <h2 className="subtitle"> {"Part 1" |> React.string} </h2>
          <textarea className="textarea" value=part1TestResult readOnly=true />
          <br />
          <h2 className="subtitle"> {"Part 2" |> React.string} </h2>
          <textarea className="textarea" value=part2TestResult readOnly=true />
        </div>
      </div>
      <div className="column is-one-quarter">
        <div className="box">
          <h1 className="title"> {"Actual Results" |> React.string} </h1>
          <br />
          <h2 className="subtitle"> {"Part 1" |> React.string} </h2>
          <textarea
            className="textarea"
            value=part1ActualResult
            readOnly=true
          />
          <br />
          <h2 className="subtitle"> {"Part 2" |> React.string} </h2>
          <textarea
            className="textarea"
            value=part2ActualResult
            readOnly=true
          />
        </div>
      </div>
    </div>
  </div>;
};
