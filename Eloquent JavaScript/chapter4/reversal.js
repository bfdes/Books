function reverseArray(array) {
  const out = []
  for(i=array.length-1; i > 0; i--) {
    out.push(array[i])
  }
  return out
}

function reverseArrayInPlace(array) {
  const len = array.length
  const mid = len / 2
  for(i=0; i < mid-1; i++) {
    const tmp = array[i]
    array[i] = array[len-1-i]
    array[len-1-i] = tmp
  }
}

console.log(reverseArray(["A", "B", "C"]));
let arrayValue = [1, 2, 3, 4, 5];
reverseArrayInPlace(arrayValue);
console.log(arrayValue);
