package reprocs

import scala.collection.mutable

import breeze.linalg.{DenseMatrix, DenseVector, norm, svd}
import breeze.numerics._
import breeze.stats.regression.leastSquares

object ReprocsUtil {
  final val AllSingularValues = 0

  def isEmpty(support: DenseVector[Boolean]) = support.length == 0

  def sumBool(vec: DenseVector[Boolean]): Int = {
    var sum = 0
    vec.foreachValue { if (_) sum += 1 }
    sum
  }

  def diffBool(v1: DenseVector[Boolean], v2: DenseVector[Boolean]): DenseVector[Boolean] = {
    if (v1.length != v2.length) {
      throw new IllegalArgumentException("vector lengths should be the same")
    }
    val result = DenseVector.zeros[Boolean](v1.length)
    for (i <- 0 until result.length) {
      result(i) = v1(i) && !v2(i)
    }
    result
  }

  def getSupportIntersect(
      supportCurrent: DenseVector[Boolean],
      supportPrevious: DenseVector[Boolean]): Double = {

    sumBool(supportCurrent :& supportPrevious).toDouble / sumBool(supportPrevious).toDouble
  }

  def getSupportDiff(
    supportCurrent: DenseVector[Boolean],
    supportPrevious: DenseVector[Boolean]): Double = {

    sumBool(diffBool(supportPrevious, supportCurrent)).toDouble / sumBool(supportCurrent).toDouble
  }

  def thresh(x: DenseVector[Double], w: Double): DenseVector[Boolean] = {
    val result = DenseVector.zeros[Boolean](x.length)
    for (ix <- 0 until x.length) {
      result(ix) = abs(x(ix)) >= w
    }
    result
  }

  /* Returns indicator vector of k largest magnitude elements of x. */
  def prune(x: DenseVector[Double], k: Int): DenseVector[Boolean] = {
    def diff(t2: (Int, Double)) = -t2._2
    val minheap = new mutable.PriorityQueue[(Int, Double)]()(Ordering.by(diff))

    for (ix <- 0 until x.length) {
      minheap.enqueue((ix, x(ix)))
      if (minheap.size > k) {
        minheap.dequeue()
      }
    }

    val result = DenseVector.zeros[Boolean](x.length)
    minheap.foreach { x => result(x._1) = true }
    result
  }

  def subLeastSquares(
      y: DenseVector[Double],
      A: DenseMatrix[Double],
      T: DenseVector[Boolean]): DenseVector[Double] = {

    leastSquares(getSubMatrix(A, T), y).coefficients

    // TODO: check against subLS.m by prac-ReProCS authors

  }

  def getSubMatrix(A: DenseMatrix[Double], T: DenseVector[Boolean]): DenseMatrix[Double] = {
    val Asub = DenseMatrix.zeros[Double](sumBool(T), A.cols)
    for (irow <- 0 until Asub.rows; icol <- 0 until Asub.cols) {
      Asub(irow, icol) = A(irow, icol)
    }
    Asub
  }

  def l1Min(
      y: DenseVector[Double],
      phi: DenseMatrix[Double],
      lowRankComponent: DenseVector[Double]): DenseVector[Double] = {

    val ksi = norm(phi * lowRankComponent, 2)

    // TODO
    DenseVector.zeros[Double](0)
  }

  def weightedL1Min(
      y: DenseVector[Double],
      phi: DenseMatrix[Double],
      ksi: Double,
      support: DenseVector[Boolean],
      lambda: Double): DenseVector[Double] = {

    // TODO
    DenseVector.zeros[Double](0)
  }

  def getNewSubspace(
      alpha: Double,
      subspace: DenseMatrix[Double],
      lowRanks: mutable.Queue[DenseVector[Double]]): DenseMatrix[Double] = {

    val lowRankMatrix: DenseMatrix[Double] = getLowRankMatrix(lowRanks)
    val projectee: DenseMatrix[Double] = DenseMatrix.eye[Double](subspace.rows) - subspace * subspace.t
    projectee * lowRankMatrix
  }

  def getLowRankMatrix(lowRanks: mutable.Queue[DenseVector[Double]]): DenseMatrix[Double] = {
    val lowRankMatrix = DenseMatrix.zeros[Double](lowRanks.head.length, lowRanks.length)
    for (icol <- lowRanks.indices) {
      lowRankMatrix(::, icol) := lowRanks(icol)
    }
    lowRankMatrix
  }

  def containsGreater(x: DenseVector[Double], s: Double): Boolean = {
    for (ix <- 0 until x.length) {
      if (x(ix) > s) {
        return true
      }
    }
    false
  }

  def countGreater(x: DenseVector[Double], s: Double): Int = {
    var result = 0
    for (ix <- 0 until x.length) {
      if (x(ix) > s) {
        result += 1
      }
    }
    result
  }

  def updateSubspaceChanges(
      k: Int,
      newSubspaceComponent: DenseMatrix[Double],
      oldSubspaceComponent: DenseMatrix[Double],
      subspaceChangesDiff: mutable.Queue[Double],
      subspaceChangesBase: mutable.Queue[Double],
      lowRanks: mutable.Queue[DenseVector[Double]],
      param: ReprocsParam): Unit = {

    val lowRankMatrix = getLowRankMatrix(lowRanks)
    if (k == 1) {
      val projectionVal: DenseMatrix[Double] =
        newSubspaceComponent * (newSubspaceComponent.t * lowRankMatrix)
      val normVal = matrixNorm(projectionVal / sqrt(param.alpha))
      subspaceChangesDiff.enqueue(normVal)
      subspaceChangesBase.enqueue(normVal)
    } else {
      val projectionBase: DenseMatrix[Double] =
        newSubspaceComponent * (newSubspaceComponent.t * lowRankMatrix)
      val normBase = matrixNorm(projectionBase)
      subspaceChangesBase.enqueue(normBase)

      val projectionDiff: DenseMatrix[Double] =
        projectionBase - oldSubspaceComponent * (oldSubspaceComponent.t * lowRankMatrix)
      val normDiff =  matrixNorm(projectionDiff)
      subspaceChangesDiff.enqueue(normDiff)
    }

    if (subspaceChangesBase.length > param.kmin) {
      subspaceChangesBase.dequeue()
    }
    if (subspaceChangesDiff.length > param.kmin) {
      subspaceChangesDiff.dequeue()
    }
  }

  def matrixNorm(mat: DenseMatrix[Double]): Double = singularValues(mat, 1)(0)

  def subspaceChangeSmall(
      subspaceChangesDiff: mutable.Queue[Double],
      subspaceChangesBase: mutable.Queue[Double],
      param: ReprocsParam): Boolean = {

    // k >= kmin and subspaceChangesXXXX has at most kmin elements, so they have length kmin
    var changeSmall = false
    for (i <- 0 until param.kmin - 1) {
      changeSmall &= subspaceChangesDiff(i + 1) / subspaceChangesBase(i) < param.subspaceChangeThreshold
    }
    changeSmall
  }

  /* Returns the largest |top| singular values. */
  def singularValues(
      mat: DenseMatrix[Double],
      top: Int = AllSingularValues): DenseVector[Double] = top match {

    case AllSingularValues =>
      val svd.SVD(_, s, _) = svd(mat)
      s

    case _ =>
      val svd.SVD(_, s, _) = svd(mat)
      s(0 until top)
  }

  /* Returns the largest |top| singular values and their associated left singular vectors. */
  def mySVD(mat: DenseMatrix[Double], top: Int = AllSingularValues):
      (DenseMatrix[Double], DenseVector[Double]) = top match {

    case AllSingularValues =>
      val svd.SVD(u, s, _) = svd(mat)
      (u, s)

    case _ =>
      val svd.SVD(u, s, _) = svd(mat)
      (u(::, 0 until top), s(0 until top))
  }
}
