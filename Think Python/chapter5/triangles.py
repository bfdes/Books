# Ex 5.3
def is_triangle(a, b, c):
  m = max(a, b, c)
  return m < a + b + c - m

if __name__ == '__main__':
  a = input('a: ')
  b = input('b: ')
  c = input('c: ')
  if(is_triangle(int(a), int(b), int(c))):
    print("Yes")
  else:
    print("No")
