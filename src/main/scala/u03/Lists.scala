package u03

import scala.annotation.tailrec

object Lists extends App :

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()

  // a companion object (i.e., module) for List
  object List:

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(h, t) if n > 0 => drop(t, n - 1)
      case Cons(h, t) if n == 0 => Cons(h, t)
      case _ => Nil()

    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Cons(h, t) => Cons(h, append(t, right))
      case Nil() => right

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case _ => Nil()

    //mapper va da elemto a elemento, la flat map vuole da elemento a lista
    //in ingresso funzione da  "x" a Lista( x mappato, nil) {case i => Cons(mapper(i), Nil()); case _ => Nil()}
    def mapNew[A, B](l: List[A])(mapper: A => B): List[B] = flatMap(l)( x => Cons(mapper(x),Nil()) )

    //passo alla flatmap una funzione che va da elemento generico a lista di elemento generico, ma solo se predicato vero, altrimenti va a lista vuota
    //{case v if pred(v) => Cons(v, Nil()); case _ => Nil()}
    def filterNew[A](l: List[A])(pred: A => Boolean): List[A] = flatMap(l)( x=>{if pred(x) then Cons(x, Nil()) else Nil()} )

  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
