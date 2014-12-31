package fpInScala.solutions.Ch3

object TreeFunctions extends App {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


  val bTree: Tree[Int] = Branch(Branch(Leaf(2), Branch(Leaf(1), Leaf(42))), Branch((Branch(Leaf(14), Leaf(12))), Leaf(99)))

  //#########Ex 3.25##############
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  //#########Ex 3.26##############
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  //#########Ex 3.27##############
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  //#########Ex 3.28##############
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  //#########Ex 3.29##############
  def fold[A, B, C](tree: Tree[A])(valueTransformer: A => B)(f: (B, B) => B): B = tree match {
    case Leaf(v) => valueTransformer(v)
    case Branch(l, r) => f(fold(l)(valueTransformer)(f), fold(r)(valueTransformer)(f))
  }

  def foldSize[A](tree: Tree[A]) = fold(tree)(x => 1)(_ + _)

  def foldMax(tree: Tree[Int]) = fold(tree)(x => x)(_ max _)

  def foldDepth[A](tree: Tree[A]) = fold(tree)(x => 1)((x, y) => (x max y) + 1)

  def mkLeaf[A](value:A):Tree[A] = Leaf(value)

  def foldMap[A,B](tree: Tree[A])(f: A => B) = fold(tree)(x => mkLeaf(f(x)))((x,y) => Branch(x,y))

}
