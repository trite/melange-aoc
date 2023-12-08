type dayItem = {
  // name: string,
  component: (module Shared.DayInfo.DayInfo),
};

let daysMap: StringMap.t(dayItem) =
  [
    // 2022
    ("/y2022/d01", {component: (module Y2022.Day01)}),
    ("/y2022/d02", {component: (module Y2022.Day02)}),
    ("/y2022/d03", {component: (module Y2022.Day03)}),
    ("/y2022/d04", {component: (module Y2022.Day04)}),
    ("/y2022/d05", {component: (module Y2022.Day05)}),
    ("/y2022/d06", {component: (module Y2022.Day06)}),
    ("/y2022/d07", {component: (module Y2022.Day07)}),
    ("/y2022/d08", {component: (module Y2022.Day08)}),
    ("/y2022/d09", {component: (module Y2022.Day09)}),
    // 2023
    ("/y2023/d01", {component: (module Y2023.Day01)}),
    ("/y2023/d02", {component: (module Y2023.Day02)}),
    ("/y2023/d03", {component: (module Y2023.Day03)}),
    ("/y2023/d04", {component: (module Y2023.Day04)}),
    ("/y2023/d05", {component: (module Y2023.Day05)}),
    ("/y2023/d06", {component: (module Y2023.Day06)}),
    ("/y2023/d07", {component: (module Y2023.Day07)}),
  ]
  |> StringMap.fromList;
