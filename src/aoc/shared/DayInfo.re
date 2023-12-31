module type DayInfo = {
  let doPart1: string => string;
  let doPart2: string => string;
  let doSandbox: option(string => string);
  let p1TestInput: string;
  let p2TestInput: string;
  let actualInput: string;
  let sandboxInput: option(string);
};
