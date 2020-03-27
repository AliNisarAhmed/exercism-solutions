export class Squares {
	constructor(num) {
		this.values = Array.from(Array(num), (x, i) => i + 1);
	}

	get sumOfSquares() {
		return this.values.reduce((acc, x) => (x ** 2) + acc, 0);
	}

	get squareOfSum() {
		return this.values.reduce((acc, x) => x + acc, 0) ** 2;
	}

	get difference() {
		return this.squareOfSum - this.sumOfSquares;
	}
}
