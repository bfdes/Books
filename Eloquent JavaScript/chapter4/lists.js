function arrayToList(array) {
  const [value, ...rest] = array
  return (value === undefined) ? null : {value, rest: arrayToList(rest)}
}

function listToArray(list) {
  if(list === null) {
    return []
  }
  const { value, rest } = list
  return [value, ...listToArray(rest)]
}

function prepend(elem, list) {
  return {value: elem, rest: list}
}

function nth(list, n) {
  // Assumes list has length of at least n+1
  const { value, rest } = list
  return (n == 0) ? value : nth(rest, n-1)
}
