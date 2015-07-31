package reprocs

import scala.collection.immutable.Queue

import breeze.linalg.{DenseMatrix, DenseVector, min, sum, svd}
import breeze.numerics.{sqrt, round}

class Reprocs(
    private val param: ReprocsParam,
    private val subspace: DenseMatrix[Double],
    private val supportCurrent: Option[DenseVector[Boolean]],
    private val supportPrevious: Option[DenseVector[Boolean]],
    private val lowRanks: Queue[DenseVector[Double]],
    private val sigMin: Double,
    private var tHat: Int,
    private var flag: Boolean) {

  def decompose(mt: DenseVector[Double], t: Int):
      (DenseVector[Boolean], DenseVector[Double], DenseVector[Double]) = {
    val (phi, y) = perpProject(mt)
    val sparseComponent = sparseRecover(t, phi, y)
    val lowRankComponent = mt - sparseComponent
    subspaceUpdate(t)

    supportCurrent match {
      case Some(support) =>
        (support, sparseComponent, lowRankComponent)
      case None =>
        throw new RuntimeException("something went wrong, support was not computed")
    }
  }

  def perpProject(mt: DenseVector[Double]): (DenseMatrix[Double], DenseVector[Double]) = {
    val phi: DenseMatrix[Double] = DenseMatrix.eye[Double](subspace.rows) - subspace * subspace.t
    val y: DenseVector[Double] = phi * mt
    (phi, y)
  }

  def sparseRecover(t: Int, phi: DenseMatrix[Double], y: DenseVector[Double]): DenseVector[Double] = {
    (supportCurrent, supportPrevious) match {
      case (None, None) => {
        // see matlab code to handle case with no support data
      }
      case (Some(supCurr), None) => {
        // see matlab code to handle case with one support datum
      }
      case (Some(supCurr), Some(supPrev)) => {
        val supportChange = sumBool(supCurr :& supPrev) / sumBool(supPrev)
        if (supportChange < Reprocs.SupportChangeThreshold) {
          // TODO: l1-minimization

          // TODO: threshold
        } else {
          // TODO: weighted l1-minimization
          // TODO: prune
          // TODO: least-squares
          // TODO: threshold
        }
        // TODO: least-squares solution to get sparse component
      }
      case _ => throw new RuntimeException("something went wrong, previous support exists but not current support")
    }

    DenseVector.zeros[Double](0)
  }

  def sumBool(vec: DenseVector[Boolean]): Int = {
    var sum = 0
    vec.foreachValue { if (_) sum += 1 }
    sum
  }

  def subspaceUpdate(t: Int): Unit = {
    // TODO
  }
}

object Reprocs {
  final val Detect = true
  final val PPCA = false
  final val SupportChangeThreshold = 0.5

  /* Initialization step. */
  def apply(dataTrain: DenseMatrix[Double], param: ReprocsParam = ReprocsParam()) = {
    val mTrain = preprocessData(dataTrain)
    val (subspace, sig) = approxBasisEnergy(mTrain / sqrt(mTrain.cols), param.b)
    val r = min(subspace.cols, round(mTrain.cols / 10.0).toInt)
    val sigMin = sig(r)

    new Reprocs(
      param = param,
      subspace = subspace,
      supportCurrent = None,
      supportPrevious = None,
      lowRanks = Queue[DenseVector[Double]](),
      sigMin = sigMin,
      tHat = mTrain.cols,
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
