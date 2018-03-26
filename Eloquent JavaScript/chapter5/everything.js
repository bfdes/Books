function some(array, test) {
  for(elem of array) {
    if(test(elem)) {
      return true
    }
  }
  return false  // n.b. Can't exploit short-circuiting if implemented with reduce
}

function every(array, test) {
  /*
  Implementation is a statement of DeMorgan's Laws.
  Assume a && b = !(!a || !b) where a, b are Boolean expressions
  Then a && (b && c)  = !(!a || !(b && c))
                      = !(!a || (!b || !c))
  a && b && c = !(!a || !b || !c) by associative property.
  */
  return !some(array, elem => !test(elem))
}