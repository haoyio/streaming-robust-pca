package reprocs

import scala.collection.mutable
import scala.collection.immutable.Queue

import breeze.linalg.{DenseMatrix, DenseVector, norm}
import breeze.numerics._
import breeze.stats.regression.leastSquares

object ReprocsUtil {
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
      lowRanks: Queue[DenseVector[Double]]): DenseMatrix[Double] = {

    //TODO
    DenseMatrix.zeros[Double](0, 0)
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

  def subspaceChangeSmall(
      k: Int,
      newSubspaceComponent: DenseMatrix[Double],
      subspaceChanges: Queue[Double],
      lowRanks: Queue[DenseVector[Double]]): Boolean = {

    var changeSmall = false

    // TODO

    changeSmall
  }
}
