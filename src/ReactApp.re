module App = {
  [@react.component]
  let make = () => {
    let url = ReasonReactRouter.useUrl();

    Router.getContent(url);
  };
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
