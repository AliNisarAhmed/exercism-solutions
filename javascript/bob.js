/* eslint-disable no-unused-vars */
//
// This is only a SKELETON file for the 'Bob' exercise. It's been provided as a
// convenience to get you started writing code faster.
//

function isQuestion(str) {
  return str[str.length - 1] === '?';
}

function filterOutNonAlpha(str) {
  return str.replace(/[^a-zA-Z]/g, '');
}

function isUpper(char) {
  return char.toUpperCase() === char;
}

function isTrue(c) {
  return c === true;
}

function isAllUpper(str) {
  str = filterOutNonAlpha(str);
  if (str === '') return false;
  return [...str].map(isUpper).every(isTrue);
}

export const hey = (message) => {
  message = message.trim();
  if (message === "") return "Fine. Be that way!";
  const isAQuestion = isQuestion(message);
  const isYelling = isAllUpper(message);
  if (isAQuestion && isYelling) return "Calm down, I know what I'm doing!";
  if (isAQuestion) return "Sure.";
  if (isYelling) return "Whoa, chill out!";
  return "Whatever.";
};
