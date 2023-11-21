module App = {
  [@react.component]
  let make = () =>
    <div>
      {[
         "Aoc path test: " ++ Aoc.aocTest,
         "This is React!",
         //  "subdirs test:" ++ Aoc.Y2022.D01.testInput,
       ]
       ->Belt.List.map(greeting => <h1> greeting->React.string </h1>)
       ->Belt.List.toArray
       ->React.array}
      <hr />
      // <pre> Aoc.Y2022.D01.testInput->React.string </pre>
      <Aoc.Y2022.D01 />
      <div> <Aoc.Y2022.D02 /> </div>
    </div>;
};

ReactDOM.querySelector("#root")
->(
    fun
    | Some(root) => ReactDOM.render(<App />, root)
    | None =>
      Js.Console.error(
        "Failed to start React: couldn't find the #root element",
      )
  );
