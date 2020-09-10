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

case class Perlin(scale: Double) extends Texture {
  val ranvec: Seq[Vec3] = (0 until point_count).map(_ => Vec3.random(-1, 1).unit_vector)

  val perm_x: IndexedSeq[Int] = Random.shuffle((0 until point_count).toIndexedSeq)
  val perm_y: IndexedSeq[Int] = Random.shuffle((0 until point_count).toIndexedSeq)
  val perm_z: IndexedSeq[Int] = Random.shuffle((0 until point_count).toIndexedSeq)

  private def trilinear_interp(c: Seq[Seq[Seq[Vec3]]], u: Double, v: Double, w: Double) = {
    val uu = u*u*(3 - 2*u)
    val vv = v*v*(3 - 2*v)
    val ww = w*w*(3 - 2*w)
    (for {
      i <- 0 until 2
      j <- 0 until 2
      k <- 0 until 2
    } yield {
      val weight_v = Vec3(u - i, v - j, w - k)
      (i*uu + (1 - i)*(1 - uu))*(j*vv + (1 - j)*(1 - vv))*(k*ww + (1 - k)*(1 - ww))*c(i)(j)(k).dot(weight_v)
    }).sum
  }

  def noise(p: Vec3): Double = {
    val u = p.x - floor(p.x)
    val v = p.y - floor(p.y)
    val w = p.z - floor(p.z)

    val i = floor(p.x).toInt
    val j = floor(p.y).toInt
    val k = floor(p.z).toInt

    val c = Seq.tabulate(2, 2, 2) { (di, dj, dk) =>
      ranvec(
        perm_x((i+di) & 255) ^
        perm_y((j+dj) & 255) ^
        perm_z((k+dk) & 255)
      );
    }

    trilinear_interp(c, u, v, w)
  }

  def turb(p: Vec3, depth: Int = 7): Double = {
    val (accum, _, _) = (0 until depth).foldLeft[(Double, Vec3, Double)]((0, p, 1)) { case ((accum, temp_p, weight), _) =>
      (
        accum + weight*noise(temp_p),
        temp_p*2,
        weight*0.5
      )
    }

    abs(accum)
  }

  def apply(uv: Vec2, p: Vec3): Vec3 = Vec3(0.5)*(1 + sin(scale*p.z + 10*turb(p)))
}

case class Simplex(size: Double) extends Texture {
  val simplex = new OpenSimplex2S(Random.nextLong())
  def apply(uv: Vec2, p: Vec3): Vec3 = simplex.noise2(size*uv.x, size*uv.y)
}

