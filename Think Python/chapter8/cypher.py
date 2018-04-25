# Ex 8.5
def rotate_word(word: str, degree: int):

  def rotate_char(char: str, degree: int):
    start = ord('a') if char.islower() else ord('A')
    shifted = start + (ord(char) + degree - start) % 26
    return chr(shifted)

  rotated = ''
  for c in word:
    rotated += rotate_char(c, degree)
  return rotated
