export class NucleotideCounts {
  static parse(str) {
    const count = {
      A: 0, C: 0, G: 0, T: 0
    };

    [...str].forEach(n => {
      if (count[n] === undefined) {
        throw new Error('Invalid nucleotide in strand');
      }
      count[n] += 1;
    });

    return Object.values(count).join('');
  }
}
