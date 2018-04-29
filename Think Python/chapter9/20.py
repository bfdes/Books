# Ex 9.1
if __name__ == '__main__':
  fin = open('words.txt')
  for line in fin:
    word = line.strip()
    if len(word) > 20:
      print(word)
