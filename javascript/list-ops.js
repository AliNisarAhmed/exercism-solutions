export class List {
  constructor(xs) {
    this.values = xs ? this.generateList(xs, []): [];
  }

  generateList([first, ...rem], acc) {
    if (!first) return acc;
    if (first instanceof List) {
      return this.generateList(rem, [...acc, ...first.values ])
    } else {
      return this.generateList(rem, [...acc, first]);
    }
  }

  append(otherList) {
    this.values = [ ...this.values, ...otherList.values ];
    return this;
  }

  concat(listOfLists) {
    return new List([ ...this.values, ...listOfLists.values ]);
  }

  filter(pred) {
    let newList = [];
    for (let elem of this.values) {
      if (pred(elem)) {
        newList.push(elem);
      }
    }
    return new List(newList);
  }

  map(mapper) {
    let newList = [];
    for (let elem of this.values) {
      newList.push(mapper(elem));
    }
    return new List(newList);
  }

  length() {
    return this.values.reduce(acc => acc + 1, 0);
  }

  foldl(reducer, init) {
    let acc = init;
    for (let elem of this.values) {
      acc = reducer(acc, elem);
    }
    return acc;
  }

  foldr(reducer, init) {
    return this.reverse().foldl(reducer, init);
  }

  reverse() {
    return new List(this.values.reverse());
  }
}
