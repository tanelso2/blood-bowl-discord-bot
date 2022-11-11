import { Option } from "@core/types/generated/option";

export enum TimeUnit {
  Milliseconds = "milliseconds",
  Seconds = "seconds",
  Minutes = "minutes",
  Hours = "hours",
  Days = "days",
  Weeks = "weeks"
}

export type UnitConversion<T> = [T, number, T];

const timeConversions: UnitConversion<TimeUnit>[] = [
  [TimeUnit.Seconds, 1000, TimeUnit.Milliseconds],
  [TimeUnit.Minutes, 60, TimeUnit.Seconds],
  [TimeUnit.Hours, 60, TimeUnit.Minutes],
  [TimeUnit.Days, 24, TimeUnit.Hours]
];

function getConversionFrom<T>(conv: UnitConversion<T>, y: T): Option<[T, number]> {
  const [x1, m, x2] = conv;
  if (x1 === y) {
    return Option.Some([x2, m])
  } else if (x2 === y) {
    return Option.Some([x1, (1.0 / m)])
  } else {
    return Option.None();
  }
}

function convert<T>(convs: UnitConversion<T>[], startVal: number, startUnit: T, endUnit: T): Option<number> {
  function helper(currVal: number, currUnit: T, visited: T[]): Option<number> {
    if (currUnit === endUnit) {
      return Option.Some(currVal);
    } else {
      const candidates = convs.map(x => getConversionFrom(x, currUnit))
                              .filter(x => x.isSome())
                              .map(x => x.unwrap())
                              .filter(([x,_]) => !visited.includes(x));
      for(const [u,v] of candidates) {
        const newVal = currVal * v;
        const newVisited = visited.concat([u]);
        const ret = helper(newVal, u, newVisited);
        if (ret.isSome()) {
          return ret;
        }
      }
      return Option.None();
    }
  }
  return helper(startVal, startUnit, [startUnit]);
}

export function convertTime(startVal: number, startUnit: TimeUnit, endUnit: TimeUnit): Option<number> {
  return convert(timeConversions, startVal, startUnit, endUnit);
}