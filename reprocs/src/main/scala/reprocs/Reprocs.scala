package reprocs

import breeze.linalg._
import breeze.numerics._

class Reprocs(
    private val param: ReprocsParam,
    private val subspace: DenseMatrix[Double],
    private val supportCurrent: DenseVector[Double],
    private val supportPrevious: DenseVector[Double],
    private val lowRank: List[DenseVector[Double]],
    private val sigMin: Double,
    private var time: Int,
    private var flag: Boolean) {

  def decompose(mt: DenseVector[Double]):
      (DenseVector[Double], DenseVector[Double], DenseVector[Double]) = {
    val (yt, phit) = perpProject(mt)
    val sparseComponent = sparseRecover(yt, phit)
    val lowRankComponent = mt - sparseComponent
    subspaceUpdate()
    (supportCurrent, sparseComponent, lowRankComponent)
  }

  def perpProject(mt: DenseVector[Double]): (DenseVector[Double], DenseMatrix[Double]) = {
    // TODO

    (DenseVector.zeros[Double](0), DenseMatrix.zeros[Double](0, 0))
  }

  def sparseRecover(y: DenseVector[Double], phi: DenseMatrix[Double]): DenseVector[Double] = {
    // TODO

    DenseVector.zeros[Double](0)
  }

  def subspaceUpdate(): Unit = {
    // TODO
  }
}

object Reprocs {
  final val Detect = true
  final val PPCA = false

  /* Initialization step. */
  def apply(dataTrain: DenseMatrix[Double], param: ReprocsParam = ReprocsParam()) = {
    val mTrain = preprocessData(dataTrain)
    val (subspace, sig) = approxBasisEnergy(mTrain / sqrt(mTrain.cols), param.b)
    val r = min(subspace.cols, round(mTrain.cols / 10.0))
    val sigMin = sig(r)

    new Reprocs(
      param = param,
      subspace = subspace,
      supportCurrent = DenseVector.zeros[Double](0),
      supportPrevious = DenseVector.zeros[Double](0),
      lowRank = Nil,
      sigMin = sigMin,
      time = mTrain.cols,
      flag = Detect)
  }

  def preprocessData(dataTrain: DenseMatrix[Double]): DenseMatrix[Double] = {
    val muTrain = for (irow <- 0 until dataTrain.rows) yield sum(dataTrain(irow, ::)) / dataTrain.cols
    val mTrain = DenseMatrix.zeros[Double](dataTrain.rows, dataTrain.cols)
    for (irow <- 0 until dataTrain.rows; icol <- 0 until dataTrain.cols) {
      mTrain(irow, icol) = dataTrain(irow, icol) - muTrain(irow)
    }
    mTrain
  }

  def approxBasisEnergy(mTrain: DenseMatrix[Double], b: Double):
      (DenseMatrix[Double], DenseVector[Double]) = {
    val svd.SVD(u, s, _) = svd(mTrain)
    val energy = sum(s :* s)
    var energySoFar = 0.0
    for (i <- 0 until s.length) {
      energySoFar += s(i) * s(i)
      if (energySoFar > b * energy) {
        return (u(::, 0 to i), s(0 to i))
      }
    }
    (u, s)
  }
}
