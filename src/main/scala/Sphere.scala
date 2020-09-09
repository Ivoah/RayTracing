import scala.math._

case class Sphere(center: Vec3, radius: Double, material: Material) extends Hittable {
  def get_uv(p: Vec3): Vec2 = {
    val phi = atan2(p.z, p.x)
    val theta = asin(p.y)
    val u = 1 - (phi + Pi) / (2 * Pi)
    val v = (theta + Pi / 2) / Pi
    Vec2(u, v)
  }

  def hit(r: Ray, t_min: Double, t_max: Double): Option[Hit] = {
    val oc = r.origin - center
    val a = r.direction.length_squared
    val half_b = oc.dot(r.direction)
    val c = oc.length_squared - radius*radius
    val discriminant = half_b*half_b - a*c
    if (discriminant > 0) {
      val t1 = (-half_b - sqrt(discriminant))/a
      if (t1 < t_max && t1 > t_min) {
        return Some(Hit(r, (r.at(t1) - center)/radius, t1, get_uv((r.at(t1) - center)/radius), this.material))
      }

      val t2 = (-half_b + sqrt(discriminant))/a
      if (t2 < t_max && t2 > t_min) {
        return Some(Hit(r, (r.at(t2) - center)/radius, t2, get_uv((r.at(t1) - center)/radius), this.material))
      }
    }
    None
  }

  def bounding_box: AABB = AABB(center - radius, center + radius)
}
