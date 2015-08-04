package reprocs

class ReprocsParam(
    val q: Double,
    val b: Double,
    val alpha: Int,
    val kmin: Int,
    val kmax: Int,
    val supportChangeThreshold: Double,
    val pruneFactor: Double,
    val subspaceChangeThreshold: Double,
    val subspaceChangeNum: Int)

object ReprocsParam {
  final val Q: Double = 1.0
  final val B: Double = 0.95
  final val Alpha: Int = 20
  final val KMin: Int = 3
  final val KMax: Int = 10

  final val SupportChangeThreshold = 0.5
  final val PruneFactor = 1.4
  final val SubspaceChangeThreshold = 0.01
  final val SubspaceChangeNum = 3

  def apply(
      q: Double = Q,
      b: Double = B,
      alpha: Int = Alpha,
      kmin: Int = KMin,
      kmax: Int = KMax,
      supportChangeThreshold: Double = SupportChangeThreshold,
      pruneFactor: Double = PruneFactor,
      subspaceChangeThreshold: Double = SubspaceChangeThreshold,
      subspaceChangeNum: Int = SubspaceChangeNum) =
    new ReprocsParam(
      q = q,
      b = b,
      alpha = alpha,
      kmin = kmin,
      kmax = kmax,
      supportChangeThreshold = supportChangeThreshold,
      pruneFactor = pruneFactor,
      subspaceChangeThreshold = subspaceChangeThreshold,
      subspaceChangeNum = subspaceChangeNum)
}
