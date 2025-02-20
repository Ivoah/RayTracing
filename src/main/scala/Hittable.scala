import java.nio.file.{Files, Path}
import scala.math._

case class Hit(ray: Ray, outward_normal: Vec3, t: Double, uv: Vec2, material: Material) {
  def position: Vec3 = ray.at(t)
  def front_face: Boolean = ray.direction.dot(outward_normal) < 0
  def normal: Vec3 = if (front_face) outward_normal else -outward_normal
}

abstract class Hittable() {
  def hit(r: Ray, t_min: Double, t_max: Double): Option[Hit]
  def bounding_box: AABB
}

//case class HittableList(objects: Hittable*) extends Hittable {
//  def hit(r: Ray, t_min: Double, t_max: Double): Option[Hit] = {
//    objects.foldLeft[Option[Hit]](None) { (prevHit: Option[Hit], obj: Hittable) =>
//      obj.hit(r, t_min, prevHit.map(_.t).getOrElse(t_max)).orElse(prevHit)
//    }
//  }
//
//  final def bounding_box: AABB = objects.map(_.bounding_box).reduce(_ + _)
//}

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
      val t2 = (-half_b + sqrt(discriminant))/a
      if (t1 < t_max && t1 > t_min) {
        Some(Hit(r, (r.at(t1) - center)/radius, t1, get_uv((r.at(t1) - center)/radius), this.material))
      } else if (t2 < t_max && t2 > t_min) {
        Some(Hit(r, (r.at(t2) - center)/radius, t2, get_uv((r.at(t1) - center)/radius), this.material))
      } else None
    } else None
  }

  def bounding_box: AABB = AABB(center - radius, center + radius)
}

case class XYRect(x0: Double, x1: Double, y0: Double, y1: Double, z: Double, material: Material) extends Hittable {
  def hit(r: Ray, t_min: Double, t_max: Double): Option[Hit] = {
    val t = (z - r.origin.z)/r.direction.z
    if (t < t_min || t > t_max) None
    else {
      val x = r.origin.x + t * r.direction.x
      val y = r.origin.y + t * r.direction.y
      if (x < x0 || x > x1 || y < y0 || y > y1) None
      else Some(Hit(r, Vec3(0, 0, 1), t, Vec2((x - x0)/(x1 - x0), (y - y0)/(y1 - y0)), material))
    }
  }
  def bounding_box: AABB = AABB(Vec3(x0, y0, z - 0.0001), Vec3(x1, y1, z + 0.0001))
}

case class XZRect(x0: Double, x1: Double, z0: Double, z1: Double, y: Double, material: Material) extends Hittable {
  def hit(r: Ray, t_min: Double, t_max: Double): Option[Hit] = {
    val t = (y - r.origin.y)/r.direction.y
    if (t < t_min || t > t_max) None
    else {
      val x = r.origin.x + t * r.direction.x
      val z = r.origin.z + t * r.direction.z
      if (x < x0 || x > x1 || z < z0 || z > z1) None
      else Some(Hit(r, Vec3(0, 0, 1), t, Vec2((x - x0)/(x1 - x0), (z - z0)/(z1 - z0)), material))
    }
  }
  def bounding_box: AABB = AABB(Vec3(x0, y - 0.0001, z0), Vec3(x1, y + 0.0001, z1))
}

case class YZRect(y0: Double, y1: Double, z0: Double, z1: Double, x: Double, material: Material) extends Hittable {
  def hit(r: Ray, t_min: Double, t_max: Double): Option[Hit] = {
    val t = (x - r.origin.x)/r.direction.x
    if (t < t_min || t > t_max) None
    else {
      val y = r.origin.y + t * r.direction.y
      val z = r.origin.z + t * r.direction.z
      if (y < y0 || y > y1 || z < z0 || z > z1) None
      else Some(Hit(r, Vec3(0, 0, 1), t, Vec2((y - y0)/(y1 - y0), (z - z0)/(z1 - z0)), material))
    }
  }
  def bounding_box: AABB = AABB(Vec3(x - 0.0001, y0, z0), Vec3(x + 0.0001, y1, z1))
}

case class Triangle(vertices: (Vec3, Vec3, Vec3), material: Material) extends Hittable {
  def hit(r: Ray, t_min: Double, t_max: Double): Option[Hit] = {
    val edgeAB = vertices._2 - vertices._1
    val edgeAC = vertices._3 - vertices._1
    val normalVector = edgeAB.cross(edgeAC)
    val ao = r.origin - vertices._1
    val dao = ao.cross(r.direction)
    val determinant = -r.direction.dot(normalVector)

    val t = ao.dot(normalVector)/determinant
    val u = edgeAC.dot(dao)/determinant
    val v = -edgeAB.dot(dao)/determinant
    val w = 1 - u - v

    if (determinant > 1e-6 && t >= t_min && t <= t_max && u >= 0 && v >= 0 && w >= 0) {
      Some(Hit(r, normalVector, t, Vec2(u, v), material))
    } else None
  }
  def bounding_box: AABB = AABB(Seq(vertices._1, vertices._2, vertices._3))
}
