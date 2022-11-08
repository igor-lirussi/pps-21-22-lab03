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

    //matcho la lista, se vuota torno None, se piena confronto l'elemento con il massimo della coda
    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, t) => max(t) match       //se la lista ha un elemento, calcolo il max della coda
        case Some(maxT) if maxT>h => Some(maxT) //se il max della coda è maggiore ritorna maxT
        case _ => Some(h)                       //se il max della coda è minore o ritorna None
      case Nil() => None                    //se lista vuota ritorno None


    import u02.Modules.Person
    def courseOfTeacher(l:List[Person]):List[String] = flatMap(l)({case Person.Teacher(n,course) =>Cons(course,Nil()); case _ => Nil()})

    //notare fun:(B,A)=>B
    //una funzione che compara una coppia e ritorna un valore
    //l'accumulatore va a sinistra ed è di tipo B (l'ordine conta nel meno) e la testa a dx di tipo A
    // (((acc) - 7) - 1) - 5  diverso da (5-(1-(7-acc)))
    //tipi diversi perchè nell uso prendo l'accumulatore di tipo B e la comparo con la testa di tipo A  e ritorna un nuovo acc di tipo B
    @tailrec
    def foldLeft[A,B](lst:List[A])(acc:B)(fun:(B,A)=>B):B = lst match
      case Cons(h, t) => foldLeft(t)(fun(acc,h))(fun) // torna il ris della funzione foldLeft applicata a tutta la coda con acc = computato acc vecchio e testa
      case Nil() => acc

    def reverse[A](lst:List[A]):List[A] = lst match
      case Cons(h,t) => append(reverse(t), Cons(h, Nil())) //alla coda reverse viene aggiunta la testa in coda
      case Nil() => Nil()

    //anzichè passare solo fun, passo una funzione che applica fun con parametri invertiti, l'accumulatore sta a destra ora
    //notare adesso fun:(A,B)=>B poichè abbiamo invertito i parametri e quindi anche i tipi
    def foldRightWrev[A,B](lst:List[A])(acc:B)(fun:(A,B)=>B):B = foldLeft(reverse(lst))(acc)((x, y) => fun(y, x))


    def foldRight[A,B](lst: List[A])(acc:B)(fun: (A,B)=>B):B= lst match
      case Cons(h, t) => fun(h, foldRight(t)(acc)(fun))
      case Nil() => acc



  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
