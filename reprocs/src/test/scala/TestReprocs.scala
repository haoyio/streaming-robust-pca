import breeze.linalg._

import reprocs._

object TestReprocs extends App {
  val reprocs = Reprocs(DenseMatrix.rand[Double](10, 10))
}
