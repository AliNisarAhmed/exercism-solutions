function calcRadius(x, y) {
  return Math.ceil(Math.sqrt(x * x + y * y));
}

var scores = {
  0: 10,
  1: 10,
  2: 5,
  3: 5,
  4: 5,
  5: 5,
  6: 1,
  7: 1,
  8: 1,
  9: 1,
  10: 1,
};

export const solve = (num1, num2) => {
  let radius = calcRadius(num1, num2);
  return scores[radius] ? scores[radius]: 0;
};
