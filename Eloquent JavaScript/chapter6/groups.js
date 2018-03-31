/*
Very inefficient implementation of a Set abstraction.
*/
class Group {
  constructor() {
    this.elements = []
  }

  has(elem) {
    return this.elements.indexOf(elem) != -1
  }

  add(elem) {
    if(!this.has(elem)) {
      this.elements.push(elem)
    }
  }

  delete(elem) {
    this.elements = this.elements.filter(e => e !== elem)
  }

  static from(iterable) {
    const group = new Group()
    for(let elem of iterable) {
      group.add(elem)
    }
    return group
  }

  [Symbol.iterator]() {
    class GroupIterator {
      constructor(group) {
        this.elements = group.elements
        this.index = 0
      }

      next() {
        if(this.index == this.elements.length) {
          return {done: true}
        }
        return {value: this.elements[this.index++], done: false}
      }
    }

    return new GroupIterator(this)
  }
}
