function getBinaryNumber(input) {
  return Number(input.toString(2));
}

export const secretHandshake = input => {
  if (typeof input !== "number") {
    throw new Error("Handshake must be a number");
  }
  let num = getBinaryNumber(input);
  let reverse = false;

  return (function recursive(num, acc) {
    if (num - 10000 >= 0) {
      reverse = !reverse;
      return recursive(num - 10000, acc);
    } else if (num - 1000 >= 0) {
      return recursive(num - 1000, [...acc, "jump"]);
    } else if (num - 100 >= 0) {
      return recursive(num - 100, [...acc, "close your eyes"]);
    } else if (num - 10 >= 0) {
      return recursive(num - 10, [...acc, "double blink"]);
    } else if (num - 1 >= 0) {
      return recursive(num - 1, [...acc, "wink"]);
    } else {
      return reverse ? acc : acc.reverse();
    }
  })(num, []);
};
