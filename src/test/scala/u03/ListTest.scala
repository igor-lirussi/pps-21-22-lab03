package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*

class ListTest:

  import List.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  @Test
  def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 1))
    assertEquals(Cons(30, Nil()), drop(l, 2))
    assertEquals(Nil(), drop(l, 5))

  @Test
  def testAppend() =
    val tail = Cons(40, Nil())
    assertEquals(append(l, tail), Cons(10, Cons(20, Cons(30, Cons(40, Nil())))))
    assertEquals(append(tail, l), Cons(40, Cons(10, Cons(20, Cons(30, Nil())))))
    assertEquals(append(l, Nil()), l)

  @Test
  def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def testMapNew() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), mapNew(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), mapNew(l)(_ + ""))

  @Test def testFilterNew() =
    assertEquals(Cons(20, Cons(30, Nil())), filterNew(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filterNew(l)(_ != 20))

  @Test def testMax() =
    assertEquals(max(Cons(10, Cons(25, Cons(20, Nil())))), Some(25))
    assertEquals(max(Nil()), None)

  @Test def testCourseOfTeacher() =
    import u02.Modules.Person
    import u02.Modules.Person.*
    val lst:List[Person] = Cons(Student("s1",1), Cons(Teacher("t1", "cr1"), Cons(Teacher("t2", "cr2"), Cons(Student("s2",2),Nil()))))
    assertEquals(courseOfTeacher(lst), Cons("cr1", Cons("cr2", Nil())))
    assertEquals(courseOfTeacher(Nil()), Nil())

  @Test def fold()=
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    //foldLeft
    assertEquals(16, foldLeft(lst)(0)(_ + _))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))
    val lst2 = Cons(true, Cons(true, Cons(false, Cons(true, Nil()))))
    assertEquals(true, foldLeft(lst2)(true)(_ || _))
    assertEquals(false, foldLeft(lst2)(true)(_ && _))
    //assertEquals(-16, foldLeft(lst)(0)(_ < _) ) impossibile
    //reverse
    assertEquals(Cons(5, Cons(1, Cons(7, Cons(3, Nil())))), reverse(lst))
    assertEquals(Nil(), reverse(Nil()))
    //foldRight
    //5-0 1-5 7--4 3-11 = (3-(7-(1-(5-0))) = -8
    assertEquals(-8, foldRightWrev(lst)(0)(_ - _))
    assertEquals(-8, foldRight(lst)(0)(_ - _))