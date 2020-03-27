export const classify = num => {
	if (num <= 0)
		throw new Error('Classification is only possible for natural numbers.');

	let sum = 0;

	for (let i = 1; i < num; i++) {
    if (num % i === 0)
      sum = sum + i;
	}

	return sum === num ? 'perfect' : sum > num ? 'abundant' : 'deficient';
};
