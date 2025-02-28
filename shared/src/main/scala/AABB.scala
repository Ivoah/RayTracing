import scala.math._

case class AABB(v_min: Vec3, v_max: Vec3) {
  @annotation.tailrec
  final def hit(r: Ray, t_min: Double, t_max: Double, dim: Int = 0): Boolean = {
    if (dim >= 3) true
    else if (t_max <= t_min) false
    else {
      val invD = 1.0/r.direction(dim)
      val (t0, t1) = if (invD >= 0) (
        (v_min(dim) - r.origin(dim))*invD,
        (v_max(dim) - r.origin(dim))*invD
      ) else (
        (v_max(dim) - r.origin(dim))*invD,
        (v_min(dim) - r.origin(dim))*invD
      )
      hit(r, max(t0, t_min), min(t1, t_max), dim + 1)
    }
  }

  def +(other: AABB): AABB = AABB(
    Vec3(
      min(v_min.x, other.v_min.x),
      min(v_min.y, other.v_min.y),
      min(v_min.z, other.v_min.z)
    ),
    Vec3(
      max(v_max.x, other.v_max.x),
      max(v_max.y, other.v_max.y),
      max(v_max.z, other.v_max.z)
    )
  )
}

object AABB {
  def apply(vertices: Seq[Vec3]): AABB = vertices.map(v => AABB(v, v)).reduce(_ + _)
}
