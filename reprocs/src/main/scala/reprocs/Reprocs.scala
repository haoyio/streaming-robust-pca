package reprocs

import scala.collection.immutable.Queue

import breeze.linalg._
import breeze.numerics.{round, sqrt}

class Reprocs(
    private val param: ReprocsParam,
    private val subspace: DenseMatrix[Double],
    private val supportCurrent: DenseVector[Boolean],
    private val supportPrevious: DenseVector[Boolean],
    private val lowRanks: Queue[DenseVector[Double]],
    private val sigMin: Double,
    private var tHat: Int,
    private var flag: Boolean) {

  def decompose(mt: DenseVector[Double], t: Int):
      (DenseVector[Boolean], DenseVector[Double], DenseVector[Double]) = {

    val (phi, y) = perpProject(mt)
    val sparseComponent = sparseRecover(t, mt, phi, y)
    val lowRankComponent = mt - sparseComponent
    subspaceUpdate(t)

    assert(!ReprocsUtil.isEmpty(supportCurrent))
    (supportCurrent, sparseComponent, lowRankComponent)
  }

  def perpProject(mt: DenseVector[Double]): (DenseMatrix[Double], DenseVector[Double]) = {
    val phi: DenseMatrix[Double] = DenseMatrix.eye[Double](subspace.rows) - subspace * subspace.t
    val y: DenseVector[Double] = phi * mt
    (phi, y)
  }

  def sparseRecover(
      t: Int,
      mt: DenseVector[Double],
      phi: DenseMatrix[Double],
      y: DenseVector[Double]): DenseVector[Double] = {

    if (  // no previous support
        ReprocsUtil.isEmpty(supportCurrent) &&
        ReprocsUtil.isEmpty(supportPrevious)) {

      // TODO: see matlab code to handle case with no previous support

    } else if (  // only one previous support
        !ReprocsUtil.isEmpty(supportCurrent) &&
        ReprocsUtil.isEmpty(supportPrevious)) {

      // TODO: see matlab code to handle case with one previous support

    } else if (  // at least two previous supports
        !ReprocsUtil.isEmpty(supportCurrent) &&
        !ReprocsUtil.isEmpty(supportPrevious)) {

      val supportChange = ReprocsUtil.getSupportIntersect(supportCurrent, supportPrevious)

      if (supportChange < Reprocs.SupportChangeThreshold) {
        val sparseCS = ReprocsUtil.l1Min(y, phi, lowRanks.head)

        supportPrevious := supportCurrent
        supportCurrent :=
          ReprocsUtil.thresh(
            x = sparseCS,
            w = param.q * sqrt(mt.t * mt / subspace.rows))
      } else {
        val sparseCS =
          ReprocsUtil.weightedL1Min(
            y = y,
            phi = phi,
            ksi = norm(phi * lowRanks.head, 2),
            support = supportCurrent,
            lambda = ReprocsUtil.getSupportDiff(supportCurrent, supportPrevious))

        val supportAdd =
          ReprocsUtil.prune(
            x = sparseCS,
            k = (Reprocs.PruneFactor * ReprocsUtil.sumBool(supportCurrent)).toInt)

        val sparseAdd = ReprocsUtil.subLeastSquares(y, phi, supportAdd)

        supportPrevious := supportCurrent
        supportCurrent :=
          ReprocsUtil.thresh(
            x = sparseAdd,
            w = param.q * sqrt(mt.t * mt / subspace.rows))
      }
    } else {
      throw new RuntimeException("previous support exists but not current support")
    }

    val sparseComponent = ReprocsUtil.subLeastSquares(y, phi, supportCurrent)
    sparseComponent
  }

  def subspaceUpdate(t: Int): Unit = {
    // TODO
  }
}

object Reprocs {
  final val Detect = true
  final val PPCA = false
  final val SupportChangeThreshold = 0.5
  final val PruneFactor = 1.4

  /* Initialization step. */
  def apply(dataTrain: DenseMatrix[Double], param: ReprocsParam = ReprocsParam()) = {
    val mTrain = preprocessData(dataTrain)
    val (subspace, sig) = approxBasisEnergy(mTrain / sqrt(mTrain.cols), param.b)
    val r = min(subspace.cols, round(mTrain.cols / 10.0).toInt)
    val sigMin = sig(r)

    new Reprocs(
      param = param,
      subspace = subspace,
      supportCurrent = emptySupport(),
      supportPrevious = emptySupport(),
      lowRanks = Queue[DenseVector[Double]](),
      sigMin = sigMin,
      tHat = mTrain.cols,
      flag = Detect)
  }

  def emptySupport() = DenseVector.zeros[Boolean](0)

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
