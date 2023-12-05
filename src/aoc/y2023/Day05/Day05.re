// TODO: remove warnings
[@ocaml.warning "-69"]
type map = {
  from: int,
  to_: int,
  range: int,
};

// TODO: remove warnings
[@ocaml.warning "-34-69"]
type almanac = {
  seeds: list(int),
  seedToSoil: list(map),
  soilToFertilizer: list(map),
  fertilizerToWater: list(map),
  waterToLight: list(map),
  lightToTemperature: list(map),
  temperatureToHumidity: list(map),
  humidityToLocation: list(map),
};

type parsingSeeds =
  (
    ~seeds: list(int),
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

// Probably going to remove these types later
//   using them to make errors from compiler easier to follow for now
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

let parseAlmanac = (input: string): almanac => {
  // This approach won't work since the `acc` signature would change with each applied value
  // let rec go = (acc: makingAlmanac, state: parsingState, lines: list(string)) => {
  //   switch (state, lines) {
  //   | (Seeds, [line, ...rest]) =>
  //     go(acc(~seeds=[1,2,3]), SeedToSoil, rest)
  //   }
  // };

  let parseSeeds =
      (acc: parsingSeeds, state: parsingState, lines: list(string))
      : result((parsingSeedToSoil, parsingState, list(string)), string) => {
    switch (state, lines) {
    | (Seeds, [line, ...rest]) =>
      switch (line |> String.splitList(~delimiter=" ")) {
      | ["seeds:", ...seedsRaw] =>
        seedsRaw
        |> List.map(
             Int.fromString >> Result.fromOption("Failed to parse seed"),
           )
        |> List.Result.sequence
        |> Result.map(seeds => (acc(~seeds), SeedToSoil, rest))
      | _ => Error("Failed to parse seeds line")
      }
    | _ => Error("Failed during parsing phase: Seeds")
    };
  };

  // TODO when resuming work:
  //   this function should work for any of the maps that need to be built
  //   will use `mapTitle` to check for title line for error handling
  //   should return result upon reaching a blank line or end of list
  let parseMap = (mapTitle: string, lines: list(string)) => {};

  let _ =
    input
    |> String.splitList(~delimiter="\n")
    |> parseSeeds(makeAlmanac, Seeds);

  {
    seeds: [],
    seedToSoil: [],
    soilToFertilizer: [],
    fertilizerToWater: [],
    waterToLight: [],
    lightToTemperature: [],
    temperatureToHumidity: [],
    humidityToLocation: [],
  };
};

let doPart1 = id;

let doPart2 = id;

let p1TestInput = Day05Data.testInput;

let p2TestInput = "Not there yet";

let actualInput = Day05Data.actualInput;
