export class GradeSchool {
  constructor() {
    this.school = {};
  }
  roster() {
    let res = {};
    for (let [k, v] of Object.entries(this.school)) {
      res[k] = [...v].sort();
    }
    return res;
  }

  add(name, grade) {
    if (this.school[grade]) {
      this.school[grade].push(name);
    } else {
      this.school[grade] = [name];
    }
  }
  grade(g) {
    if (this.school[g]) {
      return [...this.school[g] ].sort();
    } else {
      return [];
    }
  }
}
