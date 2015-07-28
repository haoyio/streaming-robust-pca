package reprocs

import breeze.linalg._
import scala.math.sqrt

class Reprocs(
    private val param: ReprocsParam,
    private val Pt: DenseMatrix[Double],
    private val Tt: DenseMatrix[Double]) {
  // TODO
}

object Reprocs {
  def apply(
      mTrain: DenseMatrix[Double],
      tTrain: Int,
      param: ReprocsParam = ReprocsParam()) = {
    val (p0, s0) = approxBasisEnergy(mTrain / sqrt(mTrain.cols), param.b)

    new Reprocs(param, p0, DenseMatrix.rand[Double](10, 10))
  }

  def approxBasisEnergy(mTrain: DenseMatrix[Double], b: Double):
      (DenseMatrix[Double], DenseVector[Double]) = {
    val svd.SVD(u, s, v) = svd(mTrain)
    val energy = sum(s :* s)
    var q = 0.0
    for (i <- 0 until s.length) {
      q += s(i) * s(i)
      if (q > b * energy) {
        return (u(::, 0 to i), s(0 to i))
      }
    }
    (u, s)
  }
}
