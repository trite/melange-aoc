type dayItem = {
  // name: string,
  component: (module Shared.DayInfo.DayInfo),
};

let daysMap: StringMap.t(dayItem) =
  [
    ("/y2022/d01", {component: (module Aoc.Y2022.Day01)}),
    ("/y2022/d02", {component: (module Aoc.Y2022.Day02)}),
    ("/y2022/d03", {component: (module Aoc.Y2022.Day03)}),
  ]
  |> StringMap.fromList;
