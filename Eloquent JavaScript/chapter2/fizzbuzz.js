// FizzBuzz
for(i=1; i < 101; i++) {
  const multipleOf3 = i % 3 == 0
  const multipleOf5 = i % 5 == 0
  if(multipleOf3 && multipleOf5) {
    console.log('FizzBuzz')
  } else if(multipleOf3) {
    console.log('Fizz')
  } else if(multipleOf5) {
    console.log('Buzz')
  } else {
    console.log(i)
  }
}
