import breeze.linalg.{DenseMatrix, DenseVector, norm}
import breeze.optimize.{DiffFunction, ProjectedQuasiNewton}

import reprocs._

object TestReprocs extends App {
  val reprocs = Reprocs(DenseMatrix.rand[Double](10, 10))
  testL1Min()

  def testL1Min(): Unit = {
    val f = new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]) =
        (norm(x, 1d), Optim.lossL1Grad(x))
    }

    val init = DenseVector(1.0, 2.0, 3.0)
    val optimizer = new ProjectedQuasiNewton(tolerance = Optim.Tolerance)
    val res = optimizer.minimize(f, init)
    assert(f(res) < Optim.Tolerance)
    println("minimizer = " + res + " minimum = " + f(res))
  }
}
