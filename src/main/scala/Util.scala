import scala.math._

object Util {
  def clamp(v: Double, low: Double, high: Double): Double = min(max(v, low), high)
  
  def clamp(v: Int, low: Int, high: Int): Int = min(max(v, low), high)

  def formatDuration(t: Double) = {
    if (t >= 3600) f"${t / 3600}%02.0fh${(t % 3600) / 60}%02.0fm${t % 60}%05.2fs"
    else f"${(t % 3600) / 60}%02.0fm${t % 60}%05.2fs"
  }

}
