package chapter12

case class Tree[+A](head: A, tail: List[Tree[A]])
