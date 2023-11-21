[@react.component]
let make = () =>
  ["Hello, world!", "Hello, ReasonReact!"]
  ->Belt.List.map(greeting => <h1> greeting->React.string </h1>)
  ->Belt.List.toArray
  ->React.array;
