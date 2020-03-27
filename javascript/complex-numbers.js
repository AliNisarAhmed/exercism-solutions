export class ComplexNumber {
	constructor(real, imag) {
		this.realNumber = real;
		this.imaginary = imag;
	}

	get real() {
		return this.realNumber;
	}

	get imag() {
		return this.imaginary;
	}

	add(c2) {
		return new ComplexNumber(this.real + c2.real, this.imag + c2.imag);
	}

	sub(c2) {
		return new ComplexNumber(this.real - c2.real, this.imag - c2.imag);
	}

	div(c2) {
		const factor = c2.real ** 2 + c2.imag ** 2;
		return new ComplexNumber(
			(this.real * c2.real + this.imag * c2.imag) / factor,
			(this.imag * c2.real - this.real * c2.imag) / factor
		);
	}

	mul(c2) {
		return new ComplexNumber(
			this.real * c2.real - this.imag * c2.imag,
			this.imag * c2.real + this.real * c2.imag
		);
	}

	get abs() {
		return Math.sqrt(this.real ** 2 + this.imag ** 2);
	}

	get conj() {
		return new ComplexNumber(this.real, this.imag === 0 ? 0 : -this.imag);
	}

	get exp() {
		return new ComplexNumber(Math.exp(this.real), 0).mul(
			new ComplexNumber(Math.cos(this.imag), Math.sin(this.imag))
		);
	}
}
