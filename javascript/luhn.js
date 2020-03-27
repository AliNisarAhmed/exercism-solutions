export const valid = input => {
  const num = stripSpaces(input);

  if (num.length <= 1)
    return false;

	if (!isValidInput(num)) return false;

	return luhnAlgo(num) % 10 === 0;
};

function stripSpaces(str) {
	return str.replace(/\s/g, '');
}

function isValidInput(str) {
	return [...str].every(c => /[0-9]/.test(c));
}

function luhnAlgo(num) {
	return [...num]
    .reverse()
    .reduce((acc, x, i) => {
      if (i % 2 === 1) {
        const double = Number(x) * 2;
        return double > 9 ?
          acc + (double - 9) :
          acc + double;
      } else {
        return acc + Number(x);
      }
    }, 0);
}
