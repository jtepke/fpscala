package fpInScala.solutions.Ch4

import scala.collection.immutable.List

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

//  def orElse[B >: A](ob: => Option[B]): Option[B] = this.getOrElse()

  def filter[A](pred: A => Boolean): Option[A] = this match {
    case Some(v: A) if (pred(v)) => Some(v)
    case _ => None
  }
}
object TestStuff extends App {
  val someVal = Some("42").flatMap(_ match {
    case "42" => Some(42)
    case _ => None
  })
  println(someVal)
}
case object None extends Option[Nothing]
case class Some[+A](value: A) extends Option[A]

object Option {
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(aVal), Some(bVal)) => Try(f(aVal, bVal))
    case _ => None
  }

  //println(map2(Some(1),Some(0))(_ / _))
  def Try[A](f: => A): Option[A] =
    try Some(f)
    catch{ case _ => None}


  def sequenceImperative[A](list: List[Option[A]]): Option[List[A]] = Some(list.map(_.getOrElse(return None)))

  def sequence[A](list: List[Option[A]]): Option[List[A]] = list.foldRight(Some(Nil): Option[List[A]]) { (x, y) =>
    map2(x, y)(_ :: _)
  }

  def traverse[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] = list.foldRight(Some(Nil): Option[List[B]]) {
    (x, y) => map2(f(x), y)(_ :: _)
  }

  //  println(traverse(List("1","2","sadsasd"))(x => Try(Integer.decode(x))))
  //  println(traverse(List("1","2"))(x => Try(Integer.decode(x))))
}