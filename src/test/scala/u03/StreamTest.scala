package u03

import org.junit.*
import org.junit.Assert.*
import u03.Streams.*
import Lists.List.*

class StreamTest:

  import Streams.*

  @Test 
  def testDrop() =
    val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
    assertEquals(Stream.toList(Stream.drop(s)(6)), Cons(6, Cons(7, Cons(8, Cons(9, Nil())))))

  @Test
  def testConstant() =
    val a = 0
    assertEquals(Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil()))))), Stream.toList(Stream.take(Stream.constant("x"))(5)))

  @Test
  def testFibonacci() =
    val a = 0
    //val fibs: Stream[Int] = Stream.fibonacci()
    //assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))), Stream.toList(Stream.take(fibs)(8)))
