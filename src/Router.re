let convertToPathString = (path: list(string)) =>
  path |> List.String.joinWith("/") |> (++)("/");

let getContent = (url: ReasonReactRouter.url) =>
  DayList.daysMap
  |> StringMap.get(url.path |> convertToPathString)
  |> Option.fold(<Home />, ({component}: DayList.dayItem) =>
       <DayComponent dayInfo=component />
     );
