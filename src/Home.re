let getName = url => {
  let urlList = url |> String.splitList(~delimiter="/");

  switch (urlList) {
  | [_, year, day]
      when
        String.startsWith(~search="y", year)
        && String.startsWith(~search="d", day) =>
    "Year "
    ++ String.sliceToEnd(1, year)
    ++ " Day "
    ++ String.sliceToEnd(1, day)

  | _ =>
    "Error determining name for this entry! ["
    ++ (urlList |> List.String.joinWith(" | "))
    ++ "]"
  };
};

[@react.component]
let make = () =>
  <div className="container mt-6 is-centered">
    <div className="box">
      <h1 className="title"> {"Advent of Code" |> React.string} </h1>
      {DayList.daysMap
       |> StringMap.toList
       |> List.map(((url, _)) =>
            <div key=url className="content">
              <a href="#" onClick={_ => ReasonReactRouter.push(url)}>
                {url |> getName |> React.string}
              </a>
            </div>
          )
       |> List.toArray
       |> React.array}
    </div>
  </div>;
