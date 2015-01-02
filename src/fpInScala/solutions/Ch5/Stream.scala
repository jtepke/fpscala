package fpInScala.solutions.Ch5

object test extends App {

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Empty => Nil: List[A]
      case Cons(h, stream) => h() :: stream().toList
    }

    def take(n: Int): Stream[A] = this match {
      case _ if (n < 1) => Empty
      case Cons(head, stream) => Stream.cons(head(), stream().take(n - 1))
    }

    def drop(n: Int): Stream[A] = this match {
      case _ if (n < 1) => this
      case Cons(head, stream) => stream().drop(n - 1)
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(head, stream) if (p(head())) => Stream.cons(head(), stream().takeWhile(p))
      case _ => Empty
    }

    def forAll(p: A => Boolean): Boolean = this match {
      case Cons(head, stream) if (p(head())) => stream().forAll(p)
      case Empty => true
      case _ => false
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(head, stream) => f(head(), stream().foldRight(z)(f))
      case _ => z
    }

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

}