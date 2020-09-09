import scala.math._
import scala.util.Random
import scala.language.implicitConversions

object Vec2 {
  implicit def intToVec2(v: Int): Vec2 = Vec2(v, v)
  implicit def doubleToVec2(v: Double): Vec2 = Vec2(v, v)

  def random: Vec2 = Vec2(Random.nextDouble(), Random.nextDouble())
  def random(min: Double, max: Double): Vec2 = Vec2(
    Random.between(min, max),
    Random.between(min, max),
  )
}

case class Vec2(x: Double, y: Double) {
  def unary_- : Vec2 = Vec2(-x, -y)

  def +(other: Vec2): Vec2 = Vec2(x + other.x, y + other.y)
  def -(other: Vec2): Vec2 = Vec2(x - other.x, y - other.y)

  def *(other: Vec2): Vec2 = Vec2(x*other.x, y*other.y)
  def /(other: Vec2): Vec2 = Vec2(x/other.x, y/other.y)

  def dot(other: Vec2): Double = x*other.x + y*other.y
  def reflect(other: Vec2): Vec2 = this - 2*dot(other)*other

  def length_squared: Double = x*x + y*y
  def length: Double = sqrt(length_squared)
  def unit_vector: Vec2 = this/length

  def apply(i: Int): Double = {
    i match {
      case 0 => x
      case 1 => y
    }
  }
}
