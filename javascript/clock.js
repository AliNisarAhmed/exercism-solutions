export class Clock {
  constructor(hour, minutes = 0) {
    let [div, rem] = this.divRem(minutes, 60); // divRem accounts for negative minutes
    if (hour < 0) {
      hour = (hour % 24) + 24; // account for negative hours
    }
    this.hour = (((hour + div) % 24) + 24) % 24; // to cater negative (hour + div)
    this.minutes = rem;
  }

  toString() {
    return `${String(this.hour).padStart(2, "0")}:${String(
      this.minutes
    ).padStart(2, "0")}`;
  }

  plus(m) {
    return new Clock(this.hour, this.minutes + m);
  }

  minus(m) {
    return new Clock(this.hour, this.minutes - m);
  }

  equals(clock2) {
    return this.hour === clock2.hour && this.minutes === clock2.minutes;
  }

  divRem(num, divisor = 60) {
    let rem;
    let div = Math.floor(num / divisor);
    if (num < 0) {
      num = divisor + (num % divisor); // normalizing negative numbers,like line#5
    }
    rem = num % divisor;
    return [div, rem];
  }
}
