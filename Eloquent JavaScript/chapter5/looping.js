function loop(first, test, update, body) {
  // Recursive implementation
  if(test(first)) {
    body(first)
    loop(update(first), test, update, body)
  }
}
