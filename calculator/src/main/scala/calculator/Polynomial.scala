package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
        Signal {
          (b() * b()) - (4 * a() * c())
        }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      if (delta() < 0) {
        Set()
      } else {
        val denom = 2 * a()
        val square = math.pow(delta(), 0.5)

        Set((square - b()) / denom, (- square - b()) / denom)
      }
    }
  }
}
