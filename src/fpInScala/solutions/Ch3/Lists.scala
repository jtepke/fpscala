
object Lists extends App {
  val testList = List(1, 2, 3, 4)

  //#########Ex 3.2##############
  println(s"tail($testList): ${tail(testList)}")

  def tail[A](list: List[A]): List[A] = list match {
    case x :: xs => xs
    case _ => Nil
  }

  println(s"setHead(42,$testList): ${setHead(42, testList)}")

  //#########Ex 3.3##############
  def setHead[A](newHead: A, list: List[A]): List[A] = newHead :: list.tail

  //#########Ex 3.4##############
  println(s"drop(2,$testList): ${drop(2, testList)}")

  def drop[A](n: Int, list: List[A]): List[A] = n match {
    case 0 => list
    case _ => drop(n - 1, tail(list))
  }

  //#########Ex 3.5##############
  println(dropWhile(testList, (a: Int) => a < 2))

  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = f(list.head) match {
    case true => dropWhile(tail(list), f)
    case _ => list
  }

  //#########Ex 3.6#############
  println(s"init($testList: ${init(testList)}")

  def init[A](list: List[A]): List[A] = list match {
    case _ :: Nil | Nil => Nil
    case x :: xs => x :: init(xs)
  }
}