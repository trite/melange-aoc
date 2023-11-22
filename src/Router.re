let getContent = (url: ReasonReactRouter.url) =>
  switch (url.path) {
  // | ["y2022", "d01"] => <Aoc.Y2022.Day01 />
  | ["y2022", "d01"] =>
    <DayComponent dayInfo=(module Aoc.Y2022.Day01.Day01) />
  | _ => <Home />
  };
