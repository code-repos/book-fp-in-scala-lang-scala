package fpinscala.datastructures

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    assert(                           List() ==       Nil, "the empty list is nil")
    assert(                 Cons(3,Nil).head ==         3, "the head of a single-element list it the element")
    assert(                 Cons(3,Nil).tail ==       Nil, "the tail of a single-element list is Nil")
    assert( Cons(1,Cons(2,Cons(3,Nil))).head ==         1, "the head of an n-element list it the 1st element")
    assert( Cons(1,Cons(2,Cons(3,Nil))).tail == List(2,3), "the tail of an n-element list is the remaining n-1 elements")

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    assert( sum(              List() ) ==  0,   "sum of empty list should be 0")
    assert( sum(             List(3) ) ==  3,   "sum of single-element list should be the element")
    assert( sum( List(1, 2, 3, 4, 5) ) == 15,   "sum of list should be sum of its elements")

    assert( product(                     List(3.0) ) ==   3.0,  "product of single-element list should be the element")
    assert( product( List(1.0, 2.0, 3.0, 4.0, 5.0) ) == 120.0,  "product of list should be product of its elements")

    // Exercise 3.2
    // Implement the function tail for removing the first element of a List. Note that the
    // function takes constant time. What are different choices you could make in your
    // implementation if the List is Nil? We’ll return to this question in the next chapter.

    def tail[A](list: List[A]): List[A] = list match {
      case Nil => throw new IllegalArgumentException("Empty list has no tail!")
      case Cons(x, xs) => xs
    }

    assert( tail(List(1,2,3)) == List(2,3), "the tail of a list is the list without its head")

    // EXERCISE 3.3
    // Using the same idea, implement the function setHead for replacing the first element
    // of a List with a different value.

    def setHead[A](list:List[A], x:A): List[A] = list match {
      case Nil => throw new IllegalArgumentException("Empty list has no head!")
      case Cons(_,xs) => Cons(x,xs)
    }

    assert(     setHead(List(1),2) == List(2),     "can change the head of a singleton list")
    assert( setHead(List(1,2,3),0) == List(0,2,3), "can change the head of a list")

    // EXERCISE 3.4
    // Generalize tail to the function drop, which removes the first n elements from a list.
    // Note that this function takes time proportional only to the number of elements being
    // dropped—we don’t need to make a copy of the entire List.

    def drop[A](l: List[A], n: Int): List[A] = (l,n) match {
      case (_,0) => l
      case (Nil,_) => throw new IllegalArgumentException("Cannot drop elements from an empty List!")
      case (Cons(_,tail),n) => drop(tail,n-1)
    }

    assert( drop(List(1,2,3),0) == List(1,2,3), "can drop one element from list")
    assert( drop(List(1,2,3),1) == List(2,3),   "can drop one element from list")
    assert( drop(List(1,2,3),2) == List(3),     "can drop two element from list")
    assert( drop(List(1,2,3),3) == Nil,         "can drop all elements from list")

    // EXERCISE 3.5
    // Implement dropWhile, which removes elements from the List prefix as long as they
    // match a predicate.

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(head, tail) =>
        if (f(head)) dropWhile(tail,f)
        else l
    }

    def isEven(n:Int): Boolean = n % 2 == 0
    assert(           dropWhile( Nil, isEven) == Nil,            "no element to drop")
    assert( dropWhile( List(1,2,3,4), isEven) == List(1,2,3,4),  "no elements are dropped")
    assert( dropWhile( List(2,3,4,5), isEven) == List(3,4,5),    "first element is dropped")
    assert( dropWhile( List(2,4,6,8), isEven) == Nil,            "all elements are dropped")
  }

