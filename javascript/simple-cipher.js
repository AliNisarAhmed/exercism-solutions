export class Cipher {
  constructor(key = 'ddddddddddddddddddddddddd') {
    this._key = key;
  }

  encode(plaintext) {
    return Array.prototype.map.call(plaintext, this.shiftChar('encode')).join('');
  }

  decode(cipher) {
    return Array.prototype.map.call(cipher, this.shiftChar('decode')).join('');
  }

  shiftChar (mode) {
    return (x, i) => {
      const index = (i % this._key.length);
      const shift = this.getNormalizedCharCode(this._key[index]);
      const letter = this.getNormalizedCharCode(x)
      return this.getCharFromCode(
        this.combineAlphabetCodes(letter, shift, mode)
      );
    };
  }

  // char -> code
  getNormalizedCharCode (char) {
    return char.charCodeAt() - 97;
  }

  // code -> char
  getCharFromCode (code) {
    return String.fromCharCode(code + 97);
  }

  // code -> code -> mode? -> code
  combineAlphabetCodes(code1, code2, mode) {
    if (mode === 'encode') {
      return (code1 + code2) % 26;
    } else if (mode === 'decode') {
      const diff = code1 - code2;
      return ((diff % 26) + 26) % 26;  // read: https://web.archive.org/web/20090717035140if_/javascript.about.com/od/problemsolving/a/modulobug.htm
    }
  }

  get key() {
    return this._key;
  }
}