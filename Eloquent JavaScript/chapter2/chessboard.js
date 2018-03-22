// Chess Board
const size = 8
let res = ''

for(i=0; i < size; i++) {
  for(j=0; j < size; j++) {
    if((i+j) % 2 == 0) {
      res += ' '
    } else {
      res += '#'
    }
  }
  res += '\n'
}
console.log(res)
