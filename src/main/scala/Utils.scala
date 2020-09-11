import scala.math._

object Utils {
  def clamp(v: Double, low: Double, high: Double): Double = min(max(v, low), high)
  def clamp(v: Int, low: Int, high: Int): Int = min(max(v, low), high)
}
