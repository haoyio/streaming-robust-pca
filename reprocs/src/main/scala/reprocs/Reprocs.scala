package reprocs

import scala.collection.mutable

import breeze.linalg._
import breeze.numerics.{round, sqrt}

class Reprocs(
    private val param: ReprocsParam,
    private val mTrainLast: DenseVector[Double],
    private var subspaceRecover: DenseMatrix[Double],
    private var subspaceProject: DenseMatrix[Double],
    private var newSubspaceComponent: DenseMatrix[Double],
    private val subspaceChangesDiff: mutable.Queue[Double],
    private val subspaceChangesBase: mutable.Queue[Double],
    private val supportCurrent: DenseVector[Boolean],
    private val supportPrevious: DenseVector[Boolean],
    private val lowRanks: mutable.Queue[DenseVector[Double]],
    private val sigMin: Double,
    private var tHat: Int,
    private var flag: Boolean,
    private var k: Int) {

  def decompose(mt: DenseVector[Double], t: Int):
      (DenseVector[Boolean], DenseVector[Double], DenseVector[Double]) = {

    val (phi, y) = perpProject(mt)
    val sparseComponent = sparseRecover(t, mt, phi, y)
    val lowRankComponent = lowRankEstimate(mt, sparseComponent)
    subspaceUpdate(t)

    assert(!ReprocsUtil.isEmpty(supportCurrent))
    (supportCurrent, sparseComponent, lowRankComponent)
  }

  def perpProject(mt: DenseVector[Double]): (DenseMatrix[Double], DenseVector[Double]) = {
    val phi: DenseMatrix[Double] =
      DenseMatrix.eye[Double](subspaceRecover.rows) - subspaceRecover * subspaceRecover.t
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

      val sparseCS = ReprocsUtil.l1Min(y, phi, mTrainLast)

      supportPrevious := supportCurrent
      supportCurrent :=
        ReprocsUtil.thresh(
          x = sparseCS,
          w = param.q * sqrt(mt.t * mt / subspaceRecover.rows))

    } else if (  // only one previous support
        !ReprocsUtil.isEmpty(supportCurrent) &&
        ReprocsUtil.isEmpty(supportPrevious)) {

      val sparseCS = ReprocsUtil.l1Min(y, phi, lowRanks.head)

      supportPrevious := supportCurrent
      supportCurrent :=
        ReprocsUtil.thresh(
          x = sparseCS,
          w = param.q * sqrt(mt.t * mt / subspaceRecover.rows))

    } else if (  // at least two previous supports
        !ReprocsUtil.isEmpty(supportCurrent) &&
        !ReprocsUtil.isEmpty(supportPrevious)) {

      val supportChange = ReprocsUtil.getSupportIntersect(supportCurrent, supportPrevious)

      if (supportChange < param.supportChangeThreshold) {
        val sparseCS = ReprocsUtil.l1Min(y, phi, lowRanks.head)

        supportPrevious := supportCurrent
        supportCurrent :=
          ReprocsUtil.thresh(
            x = sparseCS,
            w = param.q * sqrt(mt.t * mt / subspaceRecover.rows))

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
            k = (param.pruneFactor * ReprocsUtil.sumBool(supportCurrent)).toInt)

        val sparseAdd = ReprocsUtil.subLeastSquares(y, phi, supportAdd)

        supportPrevious := supportCurrent
        supportCurrent :=
          ReprocsUtil.thresh(
            x = sparseAdd,
            w = param.q * sqrt(mt.t * mt / subspaceRecover.rows))
      }
    } else {
      throw new RuntimeException("previous support exists but not current support")
    }

    val sparseComponent = ReprocsUtil.subLeastSquares(y, phi, supportCurrent)
    sparseComponent
  }

  def lowRankEstimate(
      mt: DenseVector[Double],
      sparseComponent: DenseVector[Double]): DenseVector[Double] = {

    val lowRankComponent = mt - sparseComponent
    lowRanks.enqueue(lowRankComponent)
    if (lowRanks.length > param.alpha) {
      lowRanks.dequeue()
    }
    lowRankComponent
  }

  def subspaceUpdate(t: Int): Unit = {
    val modValDetect = (t - tHat + 1) % param.alpha
    if (flag == Reprocs.Detect && modValDetect== 0) {
      // check if subspace update is required
      val svd.SVD(_, s, _) = svd(ReprocsUtil.getNewSubspace(param.alpha, subspaceProject, lowRanks))
      if (ReprocsUtil.containsGreater(s, sigMin)) {
        // set up variables for subspace update
        flag = Reprocs.PPCA
        tHat = t - param.alpha + 1
        k = 0
      }
    }

    val modValPPCA = (t - tHat + 1) % param.alpha
    if (flag == Reprocs.PPCA && modValPPCA == 0) {
      // sparse recovery subspace update
      val svd.SVD(u, s, _) = svd(ReprocsUtil.getNewSubspace(param.alpha, subspaceProject, lowRanks))
      val numSingularVectors = min(param.alpha / 3, ReprocsUtil.countGreater(s, sigMin))
      val oldSubspaceComponent = copy(newSubspaceComponent)
      newSubspaceComponent = u(::, 0 until numSingularVectors)
      subspaceRecover = DenseMatrix.horzcat(subspaceProject, newSubspaceComponent)
      k += 1

      ReprocsUtil.updateSubspaceChanges(
        k,
        newSubspaceComponent,
        oldSubspaceComponent,
        subspaceChangesDiff,
        subspaceChangesBase,
        lowRanks,
        param)

      if (
          k == param.kmax ||
          (k >= param.kmin &&
          ReprocsUtil.subspaceChangeSmall(subspaceChangesDiff, subspaceChangesBase, param))) {

        // project PCA subspace update
        subspaceProject = copy(subspaceRecover)
        flag = Reprocs.Detect
      }
    }
  }
}

object Reprocs {
  final val Detect = true
  final val PPCA = false

  /* Initialization step. */
  def apply(dataTrain: DenseMatrix[Double], param: ReprocsParam = ReprocsParam()) = {
    val mTrain = preprocessData(dataTrain)
    val (subspaceRecover, sig) = approxBasisEnergy(mTrain / sqrt(mTrain.cols), param.b)
    val r = min(subspaceRecover.cols, round(mTrain.cols / 10.0).toInt)
    val sigMin = sig(r)

    new Reprocs(
      param = param,
      mTrainLast = mTrain(::, mTrain.cols - 1),
      subspaceRecover = subspaceRecover,
      subspaceProject = subspaceRecover,
      newSubspaceComponent = emptySubspaceComponent(),
      subspaceChangesDiff = mutable.Queue[Double](),
      subspaceChangesBase = mutable.Queue[Double](),
      supportCurrent = emptySupport(),
      supportPrevious = emptySupport(),
      lowRanks = mutable.Queue[DenseVector[Double]](),
      sigMin = sigMin,
      tHat = mTrain.cols,
      flag = Detect,
      k = 0)
  }

  def emptySubspaceComponent() = DenseMatrix.zeros[Double](0, 0)

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
