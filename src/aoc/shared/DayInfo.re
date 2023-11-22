module type DayInfo = {
  // type t = {
  //   doPart1: string => string,
  //   doPart2: string => string,
  //   testInput: string,
  //   actualInput: string
  // }
  let doPart1: string => string;
  let doPart2: string => string;
  let testInput: string;
  let actualInput: string;
};
