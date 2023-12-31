type state = {
  part1TestInput: string,
  part2TestInput: string,
  actualInput: string,
  part1TestResult: string,
  part2TestResult: string,
  part1ActualResult: string,
  part2ActualResult: string,
  sandboxInput: option(string),
  sandboxResult: option(string),
};

[@deriving accessors]
type action =
  | SetPart1TestInput(string)
  | SetPart2TestInput(string)
  | SetActualInput(string)
  | SetSandboxInput(option(string));

module H1 = {
  [@react.component]
  let make = (~title: string) => {
    <h1 className="title"> {title |> React.string} </h1>;
  };
};

module H2 = {
  [@react.component]
  let make = (~title: string) => {
    <h2 className="subtitle"> {title |> React.string} </h2>;
  };
};

module Column = {
  [@react.component]
  let make = (~children, ~extra=?) => {
    let className =
      switch (extra) {
      | None => "column"
      | Some(extra) => "column " ++ extra
      };

    <div className> children </div>;
  };
};

module Columns = {
  [@react.component]
  let make = (~children, ~extra=?) => {
    let className =
      switch (extra) {
      | None => "columns"
      | Some(extra) => "columns " ++ extra
      };

    <div className> children </div>;
  };
};

module Box = {
  [@react.component]
  let make = (~children) => {
    <div className="box"> children </div>;
  };
};

module Textarea = {
  type textAreaAction =
    | OnChange(string => action)
    | OnOptionalChange(option(string) => action)
    | ReadOnly;

  [@react.component]
  let make = (~value, ~action, ~dispatch, ~rows=?) => {
    switch (action) {
    | OnChange(toApply) =>
      <textarea
        className="textarea"
        ?rows
        value
        onChange={e =>
          ReactEvent.Form.target(e)##value |> toApply |> dispatch
        }
      />
    | OnOptionalChange(toApply) =>
      <textarea
        className="textarea"
        ?rows
        value
        onChange={e =>
          Some(ReactEvent.Form.target(e)##value) |> toApply |> dispatch
        }
      />
    | ReadOnly => <textarea className="textarea" ?rows value readOnly=true />
    };
  };
};

[@react.component]
let make = (~dayInfo: (module Shared.DayInfo.DayInfo)) => {
  let (module Day) = dayInfo;

  let doSandbox = (optionalFunc: option(string => string), input: string) =>
    switch (optionalFunc) {
    | None => "Cannot run sandbox, no sandbox function provided"
    | Some(f) => input |> f
    };

  let initialState = {
    part1TestInput: Day.p1TestInput,
    part2TestInput: Day.p2TestInput,
    actualInput: Day.actualInput,
    part1TestResult: Day.p1TestInput |> Day.doPart1,
    part2TestResult: Day.p2TestInput |> Day.doPart2,
    part1ActualResult: Day.actualInput |> Day.doPart1,
    part2ActualResult: Day.actualInput |> Day.doPart2,
    sandboxInput: Day.sandboxInput,
    sandboxResult: Day.sandboxInput |> Option.map(doSandbox(Day.doSandbox)),
  };

  let reducer = (state: state, action: action): state =>
    switch (action) {
    | SetPart1TestInput(input) => {
        ...state,
        part1TestInput: input,
        part1TestResult: input |> Day.doPart1,
      }
    | SetPart2TestInput(input) => {
        ...state,
        part2TestInput: input,
        part2TestResult: input |> Day.doPart2,
      }
    | SetActualInput(input) => {
        ...state,
        actualInput: input,
        part1ActualResult: input |> Day.doPart1,
        part2ActualResult: input |> Day.doPart2,
      }
    | SetSandboxInput(None) => {
        ...state,
        sandboxInput: None,
        sandboxResult: None,
      }
    | SetSandboxInput(Some(input)) => {
        ...state,
        sandboxInput: Some(input),
        sandboxResult: Some(doSandbox(Day.doSandbox, input)),
      }
    };

  let (state, dispatch) = React.useReducer(reducer, initialState);

  <div className="container mt-6 is-centered">
    <Columns extra="is-centered">
      <Column extra="is-three-quarters">
        <a href="#" onClick={_ => ReasonReactRouter.push("/")}>
          {"<- Home" |> React.string}
        </a>
      </Column>
    </Columns>
    <Columns extra="is-centered">
      <Column extra="is-half">
        <Box>
          <H1 title="Test Input" />
          <Columns>
            <Column>
              <H2 title="Part 1" />
              <Textarea
                value={state.part1TestInput}
                action={Textarea.OnChange(setPart1TestInput)}
                dispatch
              />
            </Column>
            <br />
            <Column>
              <H2 title="Part 2" />
              <Textarea
                value={state.part2TestInput}
                action={Textarea.OnChange(setPart2TestInput)}
                dispatch
              />
            </Column>
          </Columns>
        </Box>
        {switch (state.sandboxInput) {
         | None => <div />
         | Some(input) =>
           <Box>
             <H1 title="Sandbox Input" />
             <Textarea
               value=input
               action={Textarea.OnOptionalChange(setSandboxInput)}
               dispatch
             />
           </Box>
         }}
        <Box>
          <H1 title="Actual Input" />
          <Textarea
            rows=12
            value={state.actualInput}
            action={Textarea.OnChange(setActualInput)}
            dispatch
          />
        </Box>
      </Column>
      <Column extra="is-half">
        <Box>
          <H1 title="Test Results" />
          <Columns>
            <Column>
              <H2 title="Part 1" />
              <Textarea
                value={state.part1TestResult}
                action=Textarea.ReadOnly
                dispatch
              />
            </Column>
            <Column>
              <H2 title="Part 2" />
              <Textarea
                value={state.part2TestResult}
                action=Textarea.ReadOnly
                dispatch
              />
            </Column>
          </Columns>
        </Box>
        {switch (state.sandboxResult) {
         | None => <div />
         | Some(input) =>
           <Box>
             <H1 title="Sandbox Result" />
             <Textarea value=input action=Textarea.ReadOnly dispatch />
           </Box>
         }}
        <Box>
          <H1 title="Actual Results" />
          <Columns>
            <Column>
              <H2 title="Part 1" />
              <Textarea
                value={state.part1ActualResult}
                action=Textarea.ReadOnly
                dispatch
              />
            </Column>
            <Column>
              <H2 title="Part 2" />
              <Textarea
                value={state.part2ActualResult}
                action=Textarea.ReadOnly
                dispatch
              />
            </Column>
          </Columns>
        </Box>
      </Column>
    </Columns>
  </div>;
};
