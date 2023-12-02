type state = {
  part1TestInput: string,
  part2TestInput: string,
  actualInput: string,
  part1TestResult: string,
  part2TestResult: string,
  part1ActualResult: string,
  part2ActualResult: string,
};

[@deriving accessors]
type action =
  | SetPart1TestInput(string)
  | SetPart2TestInput(string)
  | SetActualInput(string);

let reducer = (state: state, action: action): state =>
  switch (action) {
  | SetPart1TestInput(input) => {...state, part1TestInput: input}
  | SetPart2TestInput(input) => {...state, part2TestInput: input}
  | SetActualInput(input) => {...state, actualInput: input}
  };

[@react.component]
let make = (~dayInfo: (module Shared.DayInfo.DayInfo)) => {
  let (module Day) = dayInfo;

  let initialState = {
    part1TestInput: Day.p1TestInput,
    part2TestInput: Day.p2TestInput,
    actualInput: Day.actualInput,
    part1TestResult: Day.p1TestInput |> Day.doPart1,
    part2TestResult: Day.p2TestInput |> Day.doPart2,
    part1ActualResult: Day.actualInput |> Day.doPart1,
    part2ActualResult: Day.actualInput |> Day.doPart2,
  };

  let (state, dispatch) = React.useReducer(reducer, initialState);

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
          <br />
          <div className="columns">
            <div className="column">
              <h2 className="subtitle"> {"Test - Part 1" |> React.string} </h2>
              <textarea
                className="textarea"
                value={state.part1TestInput}
                onChange={e =>
                  ReactEvent.Form.target(e)##value
                  |> setPart1TestInput
                  |> dispatch
                }
              />
            </div>
            <br />
            <div className="column">
              <h2 className="subtitle"> {"Test - Part 2" |> React.string} </h2>
              <textarea
                className="textarea"
                value={state.part2TestInput}
                onChange={e =>
                  ReactEvent.Form.target(e)##value
                  |> setPart2TestInput
                  |> dispatch
                }
              />
            </div>
          </div>
          <h2 className="subtitle"> {"Actual" |> React.string} </h2>
          <textarea
            className="textarea"
            rows=10
            value={state.actualInput}
            onChange={e =>
              ReactEvent.Form.target(e)##value |> setActualInput |> dispatch
            }
          />
        </div>
      </div>
      <div className="column is-one-quarter">
        <div className="box">
          <h1 className="title"> {"Test Results" |> React.string} </h1>
          <br />
          <h2 className="subtitle"> {"Part 1" |> React.string} </h2>
          <textarea
            className="textarea"
            value={state.part1TestResult}
            readOnly=true
          />
          <br />
          <h2 className="subtitle"> {"Part 2" |> React.string} </h2>
          <textarea
            className="textarea"
            value={state.part2TestResult}
            readOnly=true
          />
        </div>
      </div>
      <div className="column is-one-quarter">
        <div className="box">
          <h1 className="title"> {"Actual Results" |> React.string} </h1>
          <br />
          <h2 className="subtitle"> {"Part 1" |> React.string} </h2>
          <textarea
            className="textarea"
            value={state.part1ActualResult}
            readOnly=true
          />
          <br />
          <h2 className="subtitle"> {"Part 2" |> React.string} </h2>
          <textarea
            className="textarea"
            value={state.part2ActualResult}
            readOnly=true
          />
        </div>
      </div>
    </div>
  </div>;
};
