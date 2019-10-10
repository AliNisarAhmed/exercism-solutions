export const validate = (num) => {
  let numArr = [...String(num)].map(Number);
  let length = numArr.length;
  return numArr.reduce((acc, x) => acc + Math.pow(x, length), 0) === num;
};
