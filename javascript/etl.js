export const transform = (obj) => {
  const result = {};
  for (let [key, array] of Object.entries(obj)) {
    for (let v of array) {
      result[v.toLowerCase()] = Number(key);
    }
  }
  return result;
};
