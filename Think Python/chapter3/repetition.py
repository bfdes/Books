# Ex 3.2
def do_twice(f, v):
  f(v)
  f(v)

def do_four(f, v):
  do_twice(f, v)
  do_twice(f, v)

def print_twice(s):
  print(s)
  print(s)


if __name__ == '__main__':
  do_twice(print_twice, 'spam')

