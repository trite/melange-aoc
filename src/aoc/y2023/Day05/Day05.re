// Using floats instead of ints because some values are greater than 32 bit int max
// Could use Int64, but switching to float is easier for now
type map = {
  to_: float,
  from: float,
  range: float,
};

type seeds = list(float);

// Seeds should be moved out of the almanac, but I want to figure the rest out before refactoring
type almanac = {
  seedToSoil: list(map),
  soilToFertilizer: list(map),
  fertilizerToWater: list(map),
  waterToLight: list(map),
  lightToTemperature: list(map),
  temperatureToHumidity: list(map),
  humidityToLocation: list(map),
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

let p1ParseSeeds =
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

let parseAlmanac =
    (parseSeeds, input: string): result((seeds, almanac), string) => {
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

  seed
  |> runMaps(almanac.seedToSoil)
  |> runMaps(almanac.soilToFertilizer)
  |> runMaps(almanac.fertilizerToWater)
  |> runMaps(almanac.waterToLight)
  |> runMaps(almanac.lightToTemperature)
  |> runMaps(almanac.temperatureToHumidity)
  |> runMaps(almanac.humidityToLocation);
};

let doWork = seedParser =>
  parseAlmanac(seedParser)
  >> Result.map(((seeds: seeds, almanac: almanac)) =>
       seeds |> List.map(mapSeed(almanac)) |> List.Float.min
     )
  >> Result.fold(
       err => "Error: " ++ err,
       Js.Json.stringifyAny >> Option.getOrThrow,
     );

let doPart1 = doWork(p1ParseSeeds);

let rec addValues = (curr: float, remaining: float, acc: list(float)) =>
  if (remaining <= 0.) {
    acc;
  } else {
    // addValues(start_ +. 1., end_, [start_, ...acc]);
    addValues(
      curr +. 1.,
      remaining -. 1.,
      [curr, ...acc],
    );
  };

let p2ParseSeeds =
    (lines: list(string)): result((seeds, list(string)), string) => {
  let getRanges = (seeds: seeds): result(seeds, string) => {
    let rec go = (acc: list(float), rest) =>
      // switch (rest |> List.take(2)) {
      // | [] => Ok(acc)
      // | [x, y] =>
      //   go(rangeFloat(x, y) |> List.concat(acc), rest |> List.drop(2))
      // // TODO: Resultify this
      // // | _ => raise(Failure("Should have an even number of seed values!"))
      // | _ => Error("Should have an even number of seed values!")
      // };
      switch (rest) {
      | [] => Ok(acc)
      // | [x, y, ...rest] => go(rangeFloat(x, y) |> List.concat(acc), rest)
      | [x, y, ...rest] => go(addValues(x, y, acc), rest)
      // TODO: Resultify this
      // | _ => raise(Failure("Should have an even number of seed values!"))
      | _ => Error("Should have an even number of seed values!")
      };

    go([], seeds);
  };

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
      |> Result.flatMap(getRanges)
      |> Result.map(seeds => (seeds, rest))
    | _ => Error("Failed to parse seeds line")
    }
  | _ => Error("Failed during parsing phase: Seeds")
  };
};

let doPart2 = doWork(p2ParseSeeds);

let p1TestInput = Day05Data.testInput;

let p2TestInput = "Not there yet";

let actualInput = Day05Data.testInput;
