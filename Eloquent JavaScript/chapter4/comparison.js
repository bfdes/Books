function deepEqual(a, b) {
  if(a === b) {
    return true
  }

  const isAObject = typeof a == 'object' && a != null
  const isBObject = typeof b == 'object' && b != null
  if(!isAObject || !isBObject) {
    return false
  }

  const aKeys = Object.keys(a)
  const bKeys = Object.keys(b)
  if(aKeys.length != bKeys.length) {
    return false
  }

  for(const key of aKeys) {
    if(!(bKeys.includes(key))) {
      return false
    }
    if(!deepEqual(a[key], b[key])) {
      return false
    }
  }
  
  return true
}
