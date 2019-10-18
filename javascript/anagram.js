export class Anagram {
  constructor(word) {
    this.word = word.toLowerCase();
  }

  matches(arr) {
    const { word } = this;
    const sortedWord = [...word].sort().join('');

    return (function recursive([first, ...rest], acc) {

      // if first is undefined, we are done, return accumulator
      if (!first) return acc;
      const lowerCased = first.toLowerCase();
      // words cannot be anagrams of their own
      if (lowerCased === word) {
        return recursive(rest, acc);
      }
      // check the length &&
      // check if the two strings are equal after sorted and lowercased
      const condition = word.length === first.length &&
        sortedWord === [...lowerCased].sort().join('');
      return condition ? recursive(rest, [...acc, first]) : recursive(rest, acc);

    })(arr, []);
  }
}
