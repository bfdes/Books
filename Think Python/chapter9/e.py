# Ex 9.2
from os import path

def has_no_e(word: str):
  for char in word:
    if char == 'e':
      return False
  return True

if __name__ == '__main__':
  sample = path.abspath(path.join(__file__, '../words.txt'))
  fin = open(sample)
  count = 0
  lines = 0
  for line in fin:
    word = line.strip()
    if has_no_e(word):
      count += 1
      print(word)
    lines += 1
  print(100 * count / lines)
    
