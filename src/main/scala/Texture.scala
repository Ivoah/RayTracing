import Perlin.point_count

import scala.math._
import scala.util.Random

trait Texture {
  def apply(uv: Vec2, p: Vec3): Vec3
}

case class SolidColor(color: Vec3) extends Texture {
  def apply(uv: Vec2, p: Vec3): Vec3 = color
}

case class Checker(size: Double, t1: Texture, t2: Texture) extends Texture {
  def apply(uv: Vec2, p: Vec3): Vec3 = {
    val sines = sin(size*p.x)*sin(size*p.y)*sin(size*p.z)
    if (sines < 0) t1(uv, p)
    else t2(uv, p)
  }
}

object Perlin {
  val point_count = 256
}

case class Perlin(size: Double) extends Texture {
  val ranfloat: Seq[Double] = (0 until point_count).map(_ => Random.nextDouble())

  val perm_x = Random.shuffle((0 until point_count).toIndexedSeq)
  val perm_y = Random.shuffle((0 until point_count).toIndexedSeq)
  val perm_z = Random.shuffle((0 until point_count).toIndexedSeq)

  def noise(p: Vec3) = {
    val u = p.x - floor(p.x)
    val v = p.y - floor(p.y)
    val w = p.z - floor(p.z)

    val i = (4*p.x).toInt & 255
    val j = (4*p.y).toInt & 255
    val k = (4*p.z).toInt & 255

    ranfloat(perm_x(i) ^ perm_y(j) ^ perm_z(k))
  }

  def apply(uv: Vec2, p: Vec3): Vec3 = Vec3(1, 1, 1)*noise(size*p)
}

case class Simplex(size: Double) extends Texture {
  val simplex = new OpenSimplex2S(Random.nextLong())
  def apply(uv: Vec2, p: Vec3): Vec3 = {
    simplex.noise2(size*uv.x, size*uv.y)
  }
}
