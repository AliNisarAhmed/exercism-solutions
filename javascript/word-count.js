export class Words {
  count(str) {
    return str.trim().split(/\s+/g).reduce((acc, x) => {
      acc[x.toLowerCase()] = (acc[x.toLowerCase()] || 0) + 1;
      return acc;
    }, Object.create(null));
  }
}
