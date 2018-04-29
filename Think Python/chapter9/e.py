# Ex 9.2
def has_no_e(word: str):
  for char in word:
    if char == 'e':
      return False
  return True

if __name__ == '__main__':
  fin = open('words.txt')
  count = 0
  lines = 0
  for line in fin:
    word = line.strip()
    if has_no_e(word):
      count += 1
      print(word)
    lines += 1
  print(100 * count / lines)
    
