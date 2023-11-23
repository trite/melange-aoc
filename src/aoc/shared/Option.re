let getOrFailWith = (failureMessage, opt) =>
  opt |> Option.foldLazy(() => raise(Failure(failureMessage)), x => x);
