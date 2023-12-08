// Using floats instead of ints because some values are greater than 32 bit int max
// Could use Int64, but switching to float is easier for now
type p1Map = {
  to_: float,
  from: float,
  range: float,
};

type seeds = list(float);

type almanac('a) = {
  seedToSoil: list('a),
  soilToFertilizer: list('a),
  fertilizerToWater: list('a),
  waterToLight: list('a),
  lightToTemperature: list('a),
  temperatureToHumidity: list('a),
  humidityToLocation: list('a),
};

let makeAlmanac =
    (
      seedToSoil,
      soilToFertilizer,
      fertilizerToWater,
      waterToLight,
      lightToTemperature,
      temperatureToHumidity,
      humidityToLocation,
    ) => {
  seedToSoil,
  soilToFertilizer,
  fertilizerToWater,
  waterToLight,
  lightToTemperature,
  temperatureToHumidity,
  humidityToLocation,
};

let p1Parse = (input: string): result((seeds, almanac(p1Map)), string) => {
  let parseSeeds =
      (lines: list(string)): result((seeds, list(string)), string) => {
    switch (lines) {
    | [line, "", ...rest] =>
      switch (line |> String.splitList(~delimiter=" ")) {
      | ["seeds:", ...seedsRaw] =>
        seedsRaw
        |> List.map(seedRaw =>
             seedRaw
             |> Float.fromString
             |> Result.fromOption(
                  "Failed to parse seed: "
                  ++ (seedRaw |> Js.Json.stringifyAny |> Option.getOrThrow),
                )
           )
        |> List.Result.sequence
        |> Result.map(seeds => (seeds, rest))
      | _ => Error("Failed to parse seeds line")
      }
    | _ => Error("Failed during parsing phase: Seeds")
    };
  };

  let parseMap = (mapTitle: string, last: bool, lines: list(string)) => {
    let verifyTitleLine = (mapTitle: string, line: string) => {
      switch (line |> String.splitList(~delimiter=" ")) {
      | [title, "map:"] when title == mapTitle => Ok()
      | x =>
        Error(
          "Failed to parse map title line: "
          ++ mapTitle
          ++ "\n"
          ++ (x |> Js.Json.stringifyAny |> Option.getOrThrow),
        )
      };
    };

    let rec go = (acc: list(p1Map), last: bool, lines: list(string)) => {
      switch (lines) {
      | [] when last => Ok((acc, []))
      | [] when !last => Error("Reached end of parsing too early")
      | ["", ...rest] when !last => Ok((acc, rest))
      | [line, ...rest] =>
        switch (line |> String.splitList(~delimiter=" ")) {
        | [to_, from, range] =>
          Float.fromString(to_)
          |> Result.fromOption(
               "Failure parsing \"to\" value in line: " ++ line,
             )
          |> Result.flatMap(to_ =>
               Float.fromString(from)
               |> Result.fromOption(
                    "Failure parsing \"from\" value in line: " ++ line,
                  )
               |> Result.map(from => (to_, from))
             )
          |> Result.flatMap(((to_, from)) =>
               Float.fromString(range)
               |> Result.fromOption(
                    "Failure parsing \"range\" value in line: " ++ line,
                  )
               |> Result.flatMap(range =>
                    go([{to_, from, range}, ...acc], last, rest)
                  )
             )
        | _ => Error("Failed to parse map line: " ++ line)
        }
      | _ => Error("Figure out naming for this issue later")
      };
    };

    switch (lines) {
    | [line, ...rest] =>
      verifyTitleLine(mapTitle, line)
      |> Result.flatMap(() => go([], last, rest))
    | _ => Error("Map title line seems to be missing")
    };
  };

  let apply = (title: string, last: bool) =>
    Result.flatMap(((seeds, builder, rest)) =>
      parseMap(title, last, rest)
      |> Result.map(((soilToFertilizer, rest)) =>
           (seeds, builder(soilToFertilizer), rest)
         )
    );

  input
  |> String.splitList(~delimiter="\n")
  |> parseSeeds
  |> Result.flatMap(((seeds, rest)) =>
       parseMap("seed-to-soil", false, rest)
       |> Result.map(((seedToSoil, rest)) =>
            (seeds, makeAlmanac(seedToSoil), rest)
          )
     )
  |> apply("soil-to-fertilizer", false)
  |> apply("fertilizer-to-water", false)
  |> apply("water-to-light", false)
  |> apply("light-to-temperature", false)
  |> apply("temperature-to-humidity", false)
  |> apply("humidity-to-location", true)
  |> Result.map(((seeds, builder, _rest)) => (seeds, builder));
};

let mapSeed = (almanac: almanac(p1Map), seed: float) => {
  let rec runMaps = (mapping: list(p1Map), seed: float) => {
    switch (mapping) {
    | [] => seed
    | [{from, to_, range}, ...rest] =>
      if (seed >= from && seed < from +. range) {
        seed +. (to_ -. from);
      } else {
        runMaps(rest, seed);
      }
    };
  };

  seed
  |> runMaps(almanac.seedToSoil)
  |> runMaps(almanac.soilToFertilizer)
  |> runMaps(almanac.fertilizerToWater)
  |> runMaps(almanac.waterToLight)
  |> runMaps(almanac.lightToTemperature)
  |> runMaps(almanac.temperatureToHumidity)
  |> runMaps(almanac.humidityToLocation);
};

let doPart1 =
  p1Parse
  >> Result.flatMap(((seeds: seeds, almanac: almanac(p1Map))) =>
       seeds
       |> List.map(mapSeed(almanac))
       |> List.Float.min
       |> Result.fromOption("Failed to find min seed value")
     )
  >> Result.fold(err => "Error: " ++ err, Float.toString);

type p2Map = {
  start_: float,
  end_: float,
  add: float,
};

type range = {
  start_: float,
  end_: float,
};

// To convert from to/from/range to start/end/add:
// start = from, end = from + range - 1, add = to - from
let p1MapToP2Map = ({to_, from, range}: p1Map): p2Map => {
  start_: from,
  end_: from +. range -. 1.,
  add: to_ -. from,
};

let p1AlmanacToP2Almanac =
    (
      {
        seedToSoil,
        soilToFertilizer,
        fertilizerToWater,
        waterToLight,
        lightToTemperature,
        temperatureToHumidity,
        humidityToLocation,
      }:
        almanac(p1Map),
    )
    : almanac(p2Map) => {
  seedToSoil: List.map(p1MapToP2Map, seedToSoil),
  soilToFertilizer: List.map(p1MapToP2Map, soilToFertilizer),
  fertilizerToWater: List.map(p1MapToP2Map, fertilizerToWater),
  waterToLight: List.map(p1MapToP2Map, waterToLight),
  lightToTemperature: List.map(p1MapToP2Map, lightToTemperature),
  temperatureToHumidity: List.map(p1MapToP2Map, temperatureToHumidity),
  humidityToLocation: List.map(p1MapToP2Map, humidityToLocation),
};

let seedsToSeedRange = (seeds: seeds): result(list(range), string) => {
  let rec go = (acc: list(range), rest) => {
    switch (rest) {
    | [] => Ok(acc)
    | [start_, range, ...rest] =>
      go([{start_, end_: start_ +. range -. 1.}, ...acc], rest)
    | _ => Error("Should have an even number of seed values!")
    };
  };

  go([], seeds);
};

type seedConversionStatus =
  | Converted(range)
  | Unconverted(range)
  | Ignore;

let checkSeedAgainstMap = (seedRange: range, map: p2Map) =>
  if (seedRange.start_ >= map.start_
      && seedRange.start_ <= map.end_
      || seedRange.end_ >= map.start_
      && seedRange.end_ <= map.end_) {
    let resultStart = Float.max(seedRange.start_, map.start_);
    let resultEnd = Float.min(seedRange.end_, map.end_);

    let result =
      Converted({start_: resultStart +. map.add, end_: resultEnd +. map.add});

    let leftoverBefore =
      if (resultStart > seedRange.start_) {
        Unconverted({start_: seedRange.start_, end_: resultStart -. 1.});
      } else {
        Ignore;
      };

    let leftoverAfter =
      if (resultEnd < seedRange.end_) {
        Unconverted({start_: resultEnd +. 1., end_: seedRange.end_});
      } else {
        Ignore;
      };

    [result, leftoverBefore, leftoverAfter] |> List.filter((!=)(Ignore));
  } else {
    [Ignore];
  };

let countUnconverted =
  List.filter(
    fun
    | Converted(_) => false
    | Unconverted(_) => true
    | Ignore => false,
  )
  >> List.length;

let countConverted =
  List.filter(
    fun
    | Converted(_) => true
    | Unconverted(_) => false
    | Ignore => false,
  )
  >> List.length;

let checkSeedAgainstMaps = (seed: range, maps: list(p2Map)) => {
  let maps = ref(maps);
  let result = ref([]);

  while (maps^ |> List.length > 0) {
    let map = maps^ |> List.head |> Option.getOrThrow;

    let checkedSeeds = checkSeedAgainstMap(seed, map);

    let convertedCount = checkedSeeds |> countConverted;

    if (convertedCount > 0) {
      result := checkedSeeds;
      maps := [];
    } else {
      maps := maps^ |> List.tail |> Option.getOrThrow;
    };
  };

  // When no maps applied to the seed, it is already good to go
  if (result^ == []) {
    result := [Converted(seed)];
  };

  result^;
};

let checkSeedAgainstMaps = (seed: range, maps: list(p2Map)) => {
  let result = ref([Unconverted(seed)]);

  while (countUnconverted(result^) > 0) {
    result^
    |> List.map(
         fun
         | Converted(range) => [Converted(range)]
         | Unconverted(range) => checkSeedAgainstMaps(range, maps)
         | Ignore => [],
       )
    |> List.foldLeft(List.concat, [])
    |> (
      newResult => {
        result := newResult;
      }
    );
  };

  result^
  |> List.map(
       fun
       | Converted(range) => [range] // Converted values have been changed appropriately
       | Unconverted(range) => [range] // Values that can't be converted are left alone but kept
       | Ignore => [],
     )
  |> List.foldLeft(List.concat, []);
};

let findMinValue = (ranges: list(range)) =>
  ranges |> List.map(({start_, _}) => start_) |> List.Float.min;

let doPart2 =
  p1Parse
  >> Result.flatMap(((seeds: seeds, almanac: almanac(p1Map))) => {
       let almanac = almanac |> p1AlmanacToP2Almanac;

       seeds
       |> seedsToSeedRange
       |> Result.map(seedRanges => (seedRanges, almanac));
     })
  >> Result.map(((seedRanges, almanac)) =>
       seedRanges
       |> List.map(checkSeedAgainstMaps(_, almanac.seedToSoil))
       |> List.foldLeft(List.concat, [])
       |> (ranges => (ranges, almanac))
     )
  >> Result.map(((seedRanges, almanac)) =>
       seedRanges
       |> List.map(checkSeedAgainstMaps(_, almanac.soilToFertilizer))
       |> List.foldLeft(List.concat, [])
       |> (ranges => (ranges, almanac))
     )
  >> Result.map(((seedRanges, almanac)) =>
       seedRanges
       |> List.map(checkSeedAgainstMaps(_, almanac.fertilizerToWater))
       |> List.foldLeft(List.concat, [])
       |> (ranges => (ranges, almanac))
     )
  >> Result.map(((seedRanges, almanac)) =>
       seedRanges
       |> List.map(checkSeedAgainstMaps(_, almanac.waterToLight))
       |> List.foldLeft(List.concat, [])
       |> (ranges => (ranges, almanac))
     )
  >> Result.map(((seedRanges, almanac)) =>
       seedRanges
       |> List.map(checkSeedAgainstMaps(_, almanac.lightToTemperature))
       |> List.foldLeft(List.concat, [])
       |> (ranges => (ranges, almanac))
     )
  >> Result.map(((seedRanges, almanac)) =>
       seedRanges
       |> List.map(checkSeedAgainstMaps(_, almanac.temperatureToHumidity))
       |> List.foldLeft(List.concat, [])
       |> (ranges => (ranges, almanac))
     )
  >> Result.map(((seedRanges, almanac)) =>
       seedRanges
       |> List.map(checkSeedAgainstMaps(_, almanac.humidityToLocation))
     )
  >> Result.map(List.foldLeft(List.concat, []) >> findMinValue)
  >> Result.fold(
       err => "Error: " ++ err,
       Js.Json.stringifyAny >> Option.getOrThrow,
     );

let doSandbox = None;

let p1TestInput = Day05Data.testInput;

let p2TestInput = Day05Data.testInput;

let actualInput = Day05Data.actualInput;

let sandboxInput = None;
