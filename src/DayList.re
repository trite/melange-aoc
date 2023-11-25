type dayItem = {
  // name: string,
  component: (module Shared.DayInfo.DayInfo),
};

let daysMap: StringMap.t(dayItem) =
  [
    ("/y2022/d01", {component: (module Y2022.Day01)}),
    ("/y2022/d02", {component: (module Y2022.Day02)}),
    ("/y2022/d03", {component: (module Y2022.Day03)}),
    ("/y2022/d04", {component: (module Y2022.Day04)}),
    ("/y2022/d05", {component: (module Y2022.Day05)}),
    ("/y2022/d06", {component: (module Y2022.Day06)}),
    ("/y2022/d07", {component: (module Y2022.Day07)}),
    ("/y2022/d08", {component: (module Y2022.Day08)}),
    // ("/y2022/d09", {component: (module Y2022.Day09)}),
    // ("/y2022/d10", {component: (module Y2022.Day10)}),
  ]
  |> StringMap.fromList;
