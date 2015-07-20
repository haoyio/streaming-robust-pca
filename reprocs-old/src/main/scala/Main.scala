import breeze.linalg._

object ReProCSUtils {
  def approxBasisEnergy (m: DenseMatrix[Double], p: Double):
      (DenseMatrix[Double], DenseVector[Double]) = {
    val svd.SVD(u,s,v) = svd(m)
    val t = sum(s :* s)
    var q = 0.0
    List.range(0,s.length).foreach(i => {
      q += s(i)*s(i)
      if (q > p*t) {
        return (u(::, 0 to i), s(0 to i));
      }
    })
    return (u, s);
  }

  def approxBasisN (m: DenseMatrix[Double], n: Int):
      (DenseMatrix[Double], DenseVector[Double]) = {
    val svd.SVD(u,s,v) = svd(m)
    return (u(::, 0 to n - 1), s(0 to n - 1))
  }
}

object ReProCS {
  def runReProCS(m: DenseMatrix[Double], tTrain: Int, q: Double, b: Double,
    alpha: Double = 20, kMin: Double = 3, kMax: Double = 10):
      (DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double]) = {
    val (p, sigma) = ReProCSUtils.approxBasisEnergy(m(::, 0 to tTrain - 1), b)

    // lots more here

    return (m, m, m)
  }

  def main(args: Array[String]) {
    val a = DenseMatrix.rand(3, 3)
    runReProCS(a, 2, 0, 0.9)
  }
}
