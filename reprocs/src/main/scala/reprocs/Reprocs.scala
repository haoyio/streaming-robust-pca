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
    val (p0, s0) = ReprocsUtil.approxBasisEnergy(mTrain / sqrt(mTrain.cols), param.b)

    new Reprocs(param, p0, DenseMatrix.rand[Double](10, 10))
  }
}
