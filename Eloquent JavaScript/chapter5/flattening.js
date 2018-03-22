function flatten(array) {
  return array.reduce((flattened, a) => flattened.concat(a), [])
}
