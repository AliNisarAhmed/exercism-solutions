export const isIsogram = (str) => {
  let counts = {};
  for (let c of str) {
    let lower = c.toLowerCase();
    if (isAlpha(lower) && counts[lower] >= 1) {
      return false;
    }
    counts[lower] = 1;
  }
  return true;
};

function isAlpha(s) {
  return s.length === 1 && s.match(/[a-z]/);
}