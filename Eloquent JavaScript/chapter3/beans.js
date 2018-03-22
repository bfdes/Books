function countBs(word) {
  let count = 0
  for(i=0; i < word.length; i++) {
    if(word[i] == 'B') {
      count++
    }
  }
  return count
}

function countChar(word, char) {
  let count = 0
  for(i=0; i < word.length; i++) {
    if(word[i] == char) {
      count++
    }
  }
  return count
}

/*
function countBs(word) {
  return countChar(word, 'B')
}
*/

