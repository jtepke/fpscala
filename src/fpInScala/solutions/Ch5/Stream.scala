package fpInScala.solutions.Ch5

sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => Nil: List[A]
    case Cons(h, stream) => h() :: stream().toList
  }

  def take(n: Int): Stream[A] = this match {
    case _ if (n < 1) => Empty
    case Cons(head, stream) => Stream.cons(head(), stream().take(n - 1))
  }

  def takeUnf(n: Int): Stream[A] = Stream.unfold((this, n)) { x => x match {
    case (Cons(head, stream), n) if (n > 0) => Some((head(), (stream(), n - 1)))
    case _ => None

  }
  }

  def drop(n: Int): Stream[A] = this match {
    case _ if (n < 1) => this
    case Cons(head, stream) => stream().drop(n - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(head, stream) if (p(head())) => Stream.cons(head(), stream().takeWhile(p))
    case _ => Empty
  }

  def forAll1(p: A => Boolean): Boolean = this match {
    case Cons(head, stream) if (p(head())) => stream().forAll1(p)
    case Empty => true
    case _ => false
  }

  def forAll2(p: A => Boolean): Boolean = foldRight(true)((x, y) => p(x) && y)

  def exists(p: A => Boolean): Boolean = foldRight(false)((x, y) => p(x) || y)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(head, stream) => f(head(), stream().foldRight(z)(f))
    case _ => z
  }

  def takeWhile_1(f: A => Boolean): Stream[A] = foldRight(Stream.empty[A]) {
    (head, stream) =>
      if (f(head)) Stream.cons(head, stream)
      else Stream.empty[A]
  }

  def takeWhileUnf(f: A => Boolean): Stream[A] = Stream.unfold((this, f)) { x =>
    x match {
      case (Cons(head, stream), p) if p(head()) => Some(head(), (stream(), p))
      case _ => None
    }
  }

  def headOption: Option[A] = foldRight(None: Option[A])((x, y) => Some(x))

  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B]) { (x, y) => Stream.cons(f(x), y)}

  def mapUnf[B](f: A => B): Stream[B] = Stream.unfold(this) { x => x match {
    case Cons(head, stream) => Some(f(head()), stream())
    case _ => None
  }
  }

  def append[B >: A](stream: => Stream[B]): Stream[B] = foldRight(stream)(Stream.cons(_, _))

  def filter(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((x, y) => if (p(x)) Stream.cons(x, y) else y)

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((x, y) => f(x).append(y))

  def zipWithUnf[B](streamB: Stream[B]): Stream[(A, B)] = Stream.unfold((this, streamB)) { x =>
    x match {
      case (Cons(headA, streamA), Cons(headB, streamB)) => Some((headA(), headB()), (streamA(), streamB()))
      case _ => None
    }
  }

  def zipAllUnf[B](streamB: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, streamB)) { x =>
    x match {
      case (Cons(headA, streamA), Cons(headB, streamB)) => Some((Some(headA()), Some(headB())), (streamA(), streamB()))
      case (Empty, Cons(headB, streamB)) => Some((None, Some(headB())), (Stream.empty, streamB()))
      case (Cons(headA, streamA), Empty) => Some((Some(headA()), None), (streamA(), Stream.empty))
      case _ => None
    }
  }

  def startsWith[B >: A](prefix: Stream[B]): Boolean = this.headOption match {
    case None => false
    case _ =>
      Stream.unfold((this, prefix)) {
        x =>
          x match {
            case (Cons(headS, tailS), Cons(headP, tailP)) if (headS() == headP()) => Some(true, (tailS(), tailP()))
            case (Cons(_, _), Cons(_, _)) => Some(false, (Empty, Empty))
            case (_, Empty) | (Empty, _) => None
          }
      }.foldRight(true)(_ && _)
  }

  def tails = Stream.unfold(this) { x =>
    x match {
      case Cons(head, stream) => Some(stream(), stream())
      case Empty => None
    }
  }.append(Stream.apply(this))

  def hasSubSequence[B >: A](s: Stream[B]): Boolean = tails exists (_ startsWith s)
}

case object Empty extends Stream[Nothing]

case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val
    tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def addSuc(i: Int, suc: Int): Stream[Int] =
      cons(i, addSuc(suc, suc + i))
    addSuc(1, 0)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).getOrElse(empty) match {
      case (value: A, sucVal: S) => cons(value, unfold(sucVal)(f))
      case _ => empty
    }

  def fibsUnf: Stream[Int] = unfold((0, 1)) {
    x => x match {
      case (x1, x2) => Some((x1, (x2, x1 + x2)))
      case _ => None
    }
  }

  def fromUnf(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))

  def constantUnf[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))

  def onesUnf: Stream[Int] = unfold(1)(_ => Some(1, 1))

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}