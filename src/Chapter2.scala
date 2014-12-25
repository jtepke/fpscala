import scala.annotation.tailrec

object Chapter2 extends App {


  def curryAdd = curry((a: Int, b: Int) => a + b)

  def addOne = curryAdd(1)

  def addTwo = compose(addOne, addOne)
  //  println(s"addTwo(2)=${addTwo(2)}")

  //#########Ex 2.5##############
  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  //#########Ex 2.4##############
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }
  //#########Ex 2.3##############
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    (a: A) => (b: B) => f(a, b)
  }

  //#########Ex 2.2##############
  //  val array = Array(1,2,3,4)
  //  println(s"isSorted(${array.toList}): ${isSorted(array,{(a: Int,b: Int) => a < b})}")

  def isSorted[A](as: Array[A], order: (A, A) => Boolean): Boolean = {
    val arrayLength: Int = as.length
    if (arrayLength < 2) true
    else {
      var i = 1
      myWhile(i < arrayLength) {
        println(s"#$i ===  ${order(as(i - 1), as(i))}")
        if (!order(as(i - 1), as(i))) return false
        i += 1
      }
      true
    }
  }

  //  var rnd = Math.random()
  //  myWhile(rnd > 0.5 || rnd < 0.4) {
  //    println(s"random: $rnd")
  //    rnd = Math.random()
  //  }

  def myWhile[A](pred: => Boolean)(body: => Unit): Unit = {
    if (pred) {
      body
      myWhile(pred)(body)
    }
  }

  //#########Ex 2.1##############
  //  for (i <- 0 to 15) println(s"fib($i)=${fib(i)}")

  def fib(n: Int): Int = {
    @tailrec
    def fibo(n: Int, next: Int, result: Int): Int = n match {
      case 0 | 1 => result
      case _ => fibo(n - 1, result, next + result)
    }
    fibo(n, 1, 1)
  }
}

