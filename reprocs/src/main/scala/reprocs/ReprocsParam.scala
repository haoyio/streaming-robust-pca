package reprocs

class ReprocsParam(
    val q: Double,
    val b: Double,
    val alpha: Int,
    val kmin: Int,
    val kmax: Int)

object ReprocsParam {
  final val Q: Double = 1.0
  final val B: Double = 0.95
  final val Alpha: Int = 20
  final val KMin: Int = 3
  final val KMax: Int = 10

  def apply(
      q: Double = Q,
      b: Double = B,
      alpha: Int = Alpha,
      kmin: Int = KMin,
      kmax: Int = KMax) =
    new ReprocsParam(q, b, alpha, kmin, kmax)
}
