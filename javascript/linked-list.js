class Node {
  constructor(value) {
    this.value = value
    this.next = null;
  }
}

export class LinkedList {
  constructor() {
    this.first = null;
    this.length = 0;
  }

  push(val) {
    let newNode = new Node(val);
    if (this.length === 0) {
      this.first = newNode;
    } else {
      let node = this.first;
      while (node.next) {
        node = node.next;
      }
      node.next = newNode;
    }
    this.length++;
    return this;
  }

  pop() {
    if (this.length === 0) {
      return null;
    } else if (this.length === 1) {
      let val = this.first.value;
      this.first = null;
      this.length--;
      return val;
    } else {
      let node = this.first;
      let prev = null;
      while (node.next) {
        prev = node;
        node = node.next
      }
      let val = node.value;
      prev.next = null;
      this.length--;
      return val;
    }
  }

  shift() {
    if (this.length === 0) {
      return null;
    } else {
      let { next, value } = this.first;
      this.first = next;
      this.length--;
      return value;
    }
  }

  unshift(val) {
    let newNode = new Node(val);
    if (this.length === 0) {
      this.first = newNode;
    } else {
      newNode.next = this.first;
      this.first = newNode;
    }
    this.length++;
    return this;
  }

  delete(val) {
    if (this.length === 0) return false;
    if (this.length === 1) {
      if (this.first.value === val) {
        this.first = null;
        this.length--;
        return true;
      }
    } else if (this.length === 2) {
      if (this.first.value === val) {
        this.first = this.first.next;
        this.length--;
        return true;
      }
      if (this.first.next.value === val) {
        this.first.next = null;
        this.length--;
        return true;
      }
    } else {
      let prev = null;
      let node = this.first;
      while (node.value !== val || node.next === null) {
        prev = node;
        node = node.next;
      }
      prev.next = node.next;
      this.length--;
      return true;
    }
    return false;
  }

  count() {
    return this.length;
  }
}