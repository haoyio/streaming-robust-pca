package reprocs

import breeze.linalg.DenseVector
import breeze.numerics.signum

object ReprocsOpt {
  final val Tolerance = 1e-6

  /* ||x||_{1} subgradient */
  def lossL1Grad(x: DenseVector[Double]): DenseVector[Double] = {
    val grad = DenseVector.zeros[Double](x.length)
    for (ix <- 0 until x.length) {
      grad(ix) = signum(x(ix))
    }
    grad
  }
}
