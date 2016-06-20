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

}
