package fpinscala.datastructures.inbuiltlist

object InbuiltList {

  assert(              List() ==      Nil,  "the empty list is nil")
  assert(       (3::Nil).head ==        3,  "the head of a single-element list it the element")
  assert(       (3::Nil).tail ==      Nil,  "the tail of a single-element list is Nil")
  assert( (1::2::3::Nil).head ==        1,  "the head of an n-element list it the 1st element")
  assert( (1::2::3::Nil).tail == List(2,3), "the tail of an n-element list is the remaining n-1 elements")


  assert(           List() ==       Nil, "the empty list is nil")
  assert(     List(3).head ==         3, "the head of a single-element list it the element")
  assert(     List(3).tail ==       Nil, "the tail of a single-element list is Nil")
  assert( List(1,2,3).head ==         1, "the head of an n-element list it the 1st element")
  assert( List(1,2,3).tail == List(2,3), "the tail of an n-element list is the remaining n-1 elements")

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case x :: xs => x + sum(xs)
  }

  assert( sum(                  Nil ) ==  0, "sum of empty list should be 0")
  assert( sum(             (3::Nil) ) ==  3, "sum of single-element list should be the element" )
  assert( sum( (1::2::3::4::5::Nil) ) == 15, "sum of list should be sum of its elements")


  assert( sum(                 Nil ) ==  0, "sum of empty list should be 0")
  assert( sum(             List(3) ) ==  3, "sum of single-element list should be the element" )
  assert( sum( List(1, 2, 3, 4, 5) ) == 15, "sum of list should be sum of its elements")

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case 0.0 :: _ => 0.0
    case x :: xs => x * product(xs)
  }

  assert( product(                     List(3.0) ) ==   3.0, "product of single-element list should be the element" )
  assert( product( List(1.0, 2.0, 3.0, 4.0, 5.0) ) == 120.0, "product of list should be product of its elements")

  // Exercise 3.2
  // Implement the function tail for removing the first element of a List. Note that the
  // function takes constant time. What are different choices you could make in your
  // implementation if the List is Nil? We’ll return to this question in the next chapter.

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case x :: xs => xs
  }

  assert(         tail(Nil) == Nil,       "the tail of the empty list is the empty list")
  assert( tail(List(1,2,3)) == List(2,3), "the tail of an n-element list is the list without its head")

}
