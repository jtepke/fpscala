
object HigherOrderFunctionsOnLists extends App {

  //#########Ex 3.7##############
  //avoid multiplications
  val intLst = List(1, 2, 3, 4, 0, 4, 5, 3)
  intLst.foldRight(1) { (a, b) => {
    if (a == 0 || b == 0) 0
    else a * b
  }
  }

  //Use exception work-around -> problem: side-effect
  class FastExitException extends Exception

  def fastExitProd(lst: List[Int]): Int = {
    try {
      lst.foldRight(1) { (a, b) => {
        if (a == 0 || b == 0) throw new FastExitException
        else {
          a * b
        }
      }
      }
    }
    catch {
      case _: FastExitException => 0
    }
  }

  //#########Ex 3.8##############
  //foldRight(zeroElem)(rightAssocOp) ---> substitute all Cons with rightAssocOp and Nil with zeroElem

  //#########Ex 3.9##############
  println(s"length($intLst): ${length(intLst)}")

  def length[A](list: List[A]): Int = list.foldRight(0)((x, y) => 1 + y)

  //#########Ex 3.10##############
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case x :: xs => foldLeft(xs, f(z, x))(f)
  }

  //#########Ex 3.11##############
  def lengthL[A](list: List[A]): Int = foldLeft(list, 0)((x, y) => x + 1)

  def sumL(list: List[Int]) = foldLeft(list, 0)(_ + _)

  def productL(list: List[Int]) = foldLeft(list, 1)(_ * _)

  //#########Ex 3.12##############
  def reverse[A](list: List[A]): List[A] = list.foldRight(Nil: List[A])((x, xs) => xs ++ List(x))

  //#########Ex 3.14##############
  def append[A](list1: List[A], list2: List[A]): List[A] = list1.foldRight(list2)(_ :: _)

  //#########Ex 3.15##############
  def flatten[A](list: List[List[A]]): List[A] = list.foldRight(Nil: List[A])((x, y) => append(x, y))

  //#########Ex 3.16##############
  def addOnes(list: List[Int]): List[Int] = map(list)(HigherOrderFunctions.addOne)

  //#########Ex 3.17##############
  def doublesToString(list: List[Double]): List[String] = map(list)(a => a.toString)

  //#########Ex 3.18##############
  def map[A, B](list: List[A])(f: A => B): List[B] = list.foldRight(Nil: List[B])((x, xs) => f(x) :: xs)

  //#########Ex 3.19##############
  def filter[A](list: List[A])(f: A => Boolean): List[A] = list.foldRight(Nil: List[A]) { (x, xs) =>
    f(x) match {
      case true => x :: xs
      case false => xs
    }
  }

  //#########Ex 3.20##############
  def flatMap[A, B](list: List[A])(f: A => List[B]) = map(list)(f)

  //#########Ex 3.21##############
  def filterWithFlatMap[A](list: List[A])(pred: A => Boolean) = flatMap(list)(x => pred(x) match {
    case true => List(x)
    case false => List()
  }).flatten

  //#########Ex 3.22##############
  def addTwoLists(list1: List[Int], list2: List[Int]): List[Int] = (list1, list2) match {
    case (x :: xs, y :: ys) => (x + y) :: addTwoLists(xs, ys)
    case _ => Nil
  }

  //#########Ex 3.23##############
  def zipWith[A, B, C](list1: List[A],list2: List[B])(f: (A, B) => C): List[C] = (list1, list2) match {
    case (x :: xs, y :: ys) =>  f(x,y) :: zipWith(xs, ys)(f)
    case _ => Nil
  }

  //#########Ex 3.24##############
  def hasSubsequence[A](list:List[A],subSeq:List[A]):Boolean = (list,subSeq) match {
    case (_,Nil) => true
    case (Nil, _) => false
    case (x::xs,y::ys) if x == y => hasSubsequence(xs,ys)
    case (x::xs,_) => hasSubsequence(xs,subSeq)
  }
  
}
