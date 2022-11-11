import { convertTime, TimeUnit } from "./conversion";

describe('convertTime', () => {
  const cases: [[number, TimeUnit, TimeUnit], number][] = [
    [[1, TimeUnit.Hours, TimeUnit.Minutes], 60],
    [[24, TimeUnit.Hours, TimeUnit.Days], 1],
    [[1, TimeUnit.Hours, TimeUnit.Seconds], 3600]
  ];
  for(const [[v, startUnit, endUnit], output] of cases) {
    it(`should convert ${v} ${startUnit} to ${output} ${endUnit}`, () => {
      convertTime(v, startUnit, endUnit).unwrap().should.eql(output);
    })
  }
})