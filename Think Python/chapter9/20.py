# Ex 9.1
from os import path

if __name__ == '__main__':
  sample = path.abspath(path.join(__file__, '../words.txt'))
  fin = open(sample)
  for line in fin:
    word = line.strip()
    if len(word) > 20:
      print(word)
