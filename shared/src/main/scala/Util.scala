import scala.math._

object Util {
  def clamp(v: Int, low: Int, high: Int): Int = min(max(v, low), high)
  def clamp(v: Double, low: Double, high: Double): Double = min(max(v, low), high)

  def wrap(v: Int, low: Int, high: Int): Int = ???
  def wrap(v: Double, low: Double, high: Double): Double = ???
}
