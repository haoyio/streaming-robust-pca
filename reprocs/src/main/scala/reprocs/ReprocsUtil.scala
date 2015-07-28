package reprocs

import breeze.linalg._

object ReprocsUtil {
  def approxBasis(m: DenseMatrix[Double], b: Double):
      (DenseMatrix[Double], DenseVector[Double]) = {
    // TODO


    (DenseMatrix.rand[Double](10, 10), DenseVector.rand[Double](10))
  }
}
