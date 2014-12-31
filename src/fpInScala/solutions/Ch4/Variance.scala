package fpInScala.solutions.Ch4

object Variance extends App {

  def variance(xs: Seq[Double]): Option[Double] = xs match {
    case Seq() => None
    case _ =>
      val length = xs.length
      val mean = xs.sum / xs.length
      Some(xs.map(x => math.pow(x - mean,2)).sum /length)
  }

  println(variance(List(1d,2d,3d,2d,1d)))
}