module SelectorComponent = {
  [@react.component]
  let make = () =>
    <div>
      <h1 className="title"> {"Advent of Code" |> React.string} </h1>
      <a href="/y2022/d01"> {"2022 day 1" |> React.string} </a>
      <button onClick={_ => ReasonReactRouter.push("/y2022/d01")}>
        {"2022 day 1" |> React.string}
      </button>
    </div>;
};

module Router = {
  [@react.component]
  let make = () => {
    let url = ReasonReactRouter.useUrl();

    switch (url.path) {
    | ["y2022", "d01"] => <Aoc.Y2022.D01 />
    | _ => <SelectorComponent />
    };
  };
  // <div>
  //   {[
  //      "Aoc path test: " ++ Aoc.aocTest,
  //      "This is React!",
  //      //  "subdirs test:" ++ Aoc.Y2022.D01.testInput,
  //    ]
  //    |> List.map(greeting => <h1> greeting->React.string </h1>)
  //    |> List.toArray
  //    //  |> List.String.
  //    |> React.array}
  //   <hr />
  //   // <pre> Aoc.Y2022.D01.testInput->React.string </pre>
  //   <Aoc.Y2022.D01 />
  //   <div> <Aoc.Y2022.D02 /> </div>
  // </div>;
};

ReactDOM.querySelector("#root")
->(
    fun
    | Some(root) => ReactDOM.render(<Router />, root)
    | None =>
      Js.Console.error(
        "Failed to start React: couldn't find the #root element",
      )
  );
