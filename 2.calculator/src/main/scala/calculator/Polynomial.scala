package calculator

import scala.math.sqrt

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal {
    b() * b() - 4 * a() * c()
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    if (delta() < 0) Set.empty
    else {
      val d = sqrt(delta())
      Set((-b() + d) / (2 * a()), (-b() - d) / (2 * a()))
    }
  }
}
