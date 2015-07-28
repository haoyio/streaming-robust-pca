package reprocs

import breeze.linalg._

object ReprocsUtil {
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
