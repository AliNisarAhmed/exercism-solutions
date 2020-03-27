const daysOfWeek = [
	'Sunday',
	'Monday',
	'Tuesday',
	'Wednesday',
	'Thursday',
	'Friday',
	'Saturday'
];

const teenths = [12, 13, 14, 15, 16, 17, 18];

const typeToDay = {
	'1st': 0,
	'2nd': 1,
	'3rd': 2,
	'4th': 3,
  '5th': 4,
};

export const meetupDay = (year, month, day, type) => {
	if (type === 'teenth') {
		const index = teenths
			.map(t => new Date(year, month, t).getDay())
			.findIndex(d => daysOfWeek[d] === day);

    return new Date(year, month, teenths[index]);

	} else {
		const fullMonth = Array.from(
			Array(31),
			(x, i) => new Date(year, month, i + 1)
		);

		const dayNumber = daysOfWeek.findIndex(d => d === day);

		const filtered = fullMonth.filter(
			d => d.getDay() === dayNumber && d.getMonth() === month
		);

		const result =
			type === 'last'
				? filtered[filtered.length - 1]
				: filtered[typeToDay[type]];

		if (!result) throw new Error('Does not exist');

		return result;
	}
};
