export const steps = (n) => {
  if (n <= 0) throw new Error('Only positive numbers are allowed');
  if (n === 1) return 0;
  return (function collatz(m, acc) {
    if (m === 1) return acc;
    return collatz(nextCollatzStep(m), acc + 1);
  })(n, 0);
};

function isEven(m) {
  return m % 2 === 0;
}

function nextCollatzStep(m) {
  if (isEven(m)) {
    return m / 2;
  } else {
    return 3 * m + 1;
  }
}
