import breeze.linalg.{DenseMatrix, DenseVector, norm}
import breeze.optimize.{DiffFunction, ProjectedQuasiNewton}

import reprocs._

object TestReprocs extends App {
  def testL1Min(): Unit = {
    val f = new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]) =
        (norm(x, 1d), ReprocsOpt.lossL1Grad(x))
    }

    val init = DenseVector(1.0, 2.0, 3.0)
    val optimizer = new ProjectedQuasiNewton(tolerance = ReprocsOpt.Tolerance)
    val res = optimizer.minimize(f, init)
    assert(f(res) < ReprocsOpt.Tolerance)
    println("minimizer = " + res + "\nminimum = " + f(res))
  }

  // TODO: solver inaccurate, figure out how to use box constraints
  def testWeightedL1Min(): Unit = {
    val f = new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
        val lambda = 0.001d
        val obj = norm(x(0 until 10), 1d) + lambda * norm(x(10 until 20), 1d)
        val grad = DenseVector.vertcat(ReprocsOpt.lossL1Grad(x(0 until 10)), lambda * ReprocsOpt.lossL1Grad(x(10 until 20)))
        (obj, grad)
      }
    }

    val init = DenseVector.rand[Double](20)
    val optimizer = new ProjectedQuasiNewton(tolerance = ReprocsOpt.Tolerance)
    val res = optimizer.minimize(f, init)
    // assert(f(res) < ReprocsOpt.Tolerance)
    println("minimizer = " + res + "\nminimum = " + f(res))
  }

  // main script begins here
  val reprocs = Reprocs(DenseMatrix.rand[Double](10, 10))
  testL1Min()
  testWeightedL1Min()
}
