[@react.component]
let make = () =>
  <div>
    <h1 className="title"> {"Advent of Code" |> React.string} </h1>
    <a href="/y2022/d01"> {"2022 day 1" |> React.string} </a>
    <button onClick={_ => ReasonReactRouter.push("/y2022/d01")}>
      {"2022 day 1" |> React.string}
    </button>
  </div>;
