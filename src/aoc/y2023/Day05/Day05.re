// Using floats instead of ints because some values are greater than 32 bit int max
// Could use Int64, but switching to float is easier for now
type map = {
  to_: float,
  from: float,
  range: float,
};

// Seeds should be moved out of the almanac, but I want to figure the rest out before refactoring
type almanac = {
  seeds: list(float),
  seedToSoil: list(map),
  soilToFertilizer: list(map),
  fertilizerToWater: list(map),
  waterToLight: list(map),
  lightToTemperature: list(map),
  temperatureToHumidity: list(map),
  humidityToLocation: list(map),
};

// Probably going to remove these types later
//   using them to make errors from compiler easier to follow for now
type parsingSeeds =
  (
    ~seeds: list(float),
    ~seedToSoil: list(map),
    ~soilToFertilizer: list(map),
    ~fertilizerToWater: list(map),
    ~waterToLight: list(map),
    ~lightToTemperature: list(map),
    ~temperatureToHumidity: list(map),
    ~humidityToLocation: list(map),
    unit
  ) =>
  almanac;

type parsingSeedToSoil =
  (
    ~seedToSoil: list(map),
    ~soilToFertilizer: list(map),
    ~fertilizerToWater: list(map),
    ~waterToLight: list(map),
    ~lightToTemperature: list(map),
    ~temperatureToHumidity: list(map),
    ~humidityToLocation: list(map),
    unit
  ) =>
  almanac;

let makeAlmanac: parsingSeeds =
  (
    ~seeds,
    ~seedToSoil,
    ~soilToFertilizer,
    ~fertilizerToWater,
    ~waterToLight,
    ~lightToTemperature,
    ~temperatureToHumidity,
    ~humidityToLocation,
    (),
  ) => {
    seeds,
    seedToSoil,
    soilToFertilizer,
    fertilizerToWater,
    waterToLight,
    lightToTemperature,
    temperatureToHumidity,
    humidityToLocation,
  };

// TODO: remove warnings
[@ocaml.warning "-34-37-69"]
type parsingState =
  | Seeds
  | SeedToSoil
  | SoilToFertilizer
  | FertilizerToWater
  | WaterToLight
  | LightToTemperature
  | TemperatureToHumidity
  | HumidityToLocation;

let parseAlmanac = (input: string): result(almanac, string) => {
  let parseSeeds =
      (acc: parsingSeeds, state: parsingState, lines: list(string))
      : result((parsingSeedToSoil, parsingState, list(string)), string) => {
    switch (state, lines) {
    | (Seeds, [line, "", ...rest]) =>
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
        |> Result.map(seeds => (acc(~seeds), SeedToSoil, rest))
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

    let rec go = (acc: list(map), last: bool, lines: list(string)) => {
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

  input
  |> String.splitList(~delimiter="\n")
  |> parseSeeds(makeAlmanac, Seeds)
  |> Result.flatMap(((builder, _state, rest)) =>
       parseMap("seed-to-soil", false, rest)
       |> Result.map(((seedToSoil, rest)) => (builder(~seedToSoil), rest))
     )
  |> Result.flatMap(((builder, rest)) =>
       parseMap("soil-to-fertilizer", false, rest)
       |> Result.map(((soilToFertilizer, rest)) =>
            (builder(~soilToFertilizer), rest)
          )
     )
  |> Result.flatMap(((builder, rest)) =>
       parseMap("fertilizer-to-water", false, rest)
       |> Result.map(((fertilizerToWater, rest)) =>
            (builder(~fertilizerToWater), rest)
          )
     )
  |> Result.flatMap(((builder, rest)) =>
       parseMap("water-to-light", false, rest)
       |> Result.map(((waterToLight, rest)) =>
            (builder(~waterToLight), rest)
          )
     )
  |> Result.flatMap(((builder, rest)) =>
       parseMap("light-to-temperature", false, rest)
       |> Result.map(((lightToTemperature, rest)) =>
            (builder(~lightToTemperature), rest)
          )
     )
  |> Result.flatMap(((builder, rest)) =>
       parseMap("temperature-to-humidity", false, rest)
       |> Result.map(((temperatureToHumidity, rest)) =>
            (builder(~temperatureToHumidity), rest)
          )
     )
  |> Result.flatMap(((builder, rest)) =>
       parseMap("humidity-to-location", true, rest)
       |> Result.map(((humidityToLocation, rest)) =>
            (builder(~humidityToLocation), rest)
          )
     )
  |> Result.map(((builder, _rest)) => builder());
};

let mapSeed = (almanac: almanac, seed: float) => {
  let rec runMaps = (mapping: list(map), seed: float) => {
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

  runMaps(almanac.seedToSoil, seed)
  |> runMaps(almanac.soilToFertilizer)
  |> runMaps(almanac.fertilizerToWater)
  |> runMaps(almanac.waterToLight)
  |> runMaps(almanac.lightToTemperature)
  |> runMaps(almanac.temperatureToHumidity)
  |> runMaps(almanac.humidityToLocation);
};

let doPart1 =
  parseAlmanac
  >> Result.map(almanac =>
       almanac.seeds |> List.map(mapSeed(almanac)) |> List.Float.min
     )
  >> Result.fold(
       err => "Error: " ++ err,
       Js.Json.stringifyAny >> Option.getOrThrow,
     );

let doPart2 = id;

let p1TestInput = Day05Data.testInput;

let p2TestInput = "Not there yet";

let actualInput = Day05Data.actualInput;
