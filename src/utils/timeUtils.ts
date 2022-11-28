import { UnitConversion, convert } from "./conversion";

export enum TimeUnit {
  Milliseconds = "milliseconds",
  Seconds = "seconds",
  Minutes = "minutes",
  Hours = "hours",
  Days = "days",
  Weeks = "weeks"
}

const timeConversions: UnitConversion<TimeUnit>[] = [
  [TimeUnit.Seconds, 1000, TimeUnit.Milliseconds],
  [TimeUnit.Minutes, 60, TimeUnit.Seconds],
  [TimeUnit.Hours, 60, TimeUnit.Minutes],
  [TimeUnit.Days, 24, TimeUnit.Hours],
  [TimeUnit.Weeks, 7, TimeUnit.Days]
];


export function convertTime(startVal: number, startUnit: TimeUnit, endUnit: TimeUnit): Option<number> {
  return convert(timeConversions, startVal, startUnit, endUnit);
}

export function timeStringDaysElapsed(time: number, unit: TimeUnit): string {
    const days = Math.floor(convertTime(time, unit, TimeUnit.Days).unwrap());
    if (days < 1) {
        return `less than a day`;
    } else if (days < 7) {
        return `only ${days} days`;
    } else {
        return `${days} days`;
    }
}