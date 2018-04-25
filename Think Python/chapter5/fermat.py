# Ex 5.2
def check_fermat(a, b, c, n):
  if(n > 2 and pow(a, n) + pow(b, n) == pow(c, n)):
    print('Holy smokes Fermat was wrong')
  else:
    print("No that doesn't work")

if __name__ == '__main__':
  a = input('a : ')
  b = input('b : ')
  c = input('c : ')
  n = input('n : ')
  check_fermat(int(a), int(b), int(c), int(n))
