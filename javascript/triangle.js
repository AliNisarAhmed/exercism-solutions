export class Triangle {
  constructor(s1, s2, s3) {
    this.sides = [s1, s2, s3];
  }

  kind() {
    if (this.sides.some(e => e <= 0)) {
      throw new Error("Side cannot be equal to or less than zero");
    }
    if (this.validate()) {
      throw new Error("Invalid triangle");
    }
    let size = new Set(this.sides).size;
    let kinds = {
      1: "equilateral",
      2: "isosceles",
      3: "scalene"
    };
    return kinds[size];
  }

  validate() {
    let [s1, s2, s3] = this.sides;
    return s1 + s2 < s3 || s2 + s3 < s1 || s1 + s3 < s2;
  }
}
