case class Hit(ray: Ray, outward_normal: Vec3, t: Double, uv: Vec2, material: Material) {
  def position: Vec3 = ray.at(t)
  def front_face: Boolean = ray.direction.dot(outward_normal) < 0
  def normal: Vec3 = if (front_face) outward_normal else -outward_normal
}

abstract class Hittable() {
  def hit(r: Ray, t_min: Double, t_max: Double): Option[Hit]
  def bounding_box: AABB
}

case class HittableList(objects: Hittable*) extends Hittable {
  def hit(r: Ray, t_min: Double, t_max: Double): Option[Hit] = {
    objects.foldLeft[Option[Hit]](None) { (prevHit: Option[Hit], obj: Hittable) =>
      obj.hit(r, t_min, prevHit.map(_.t).getOrElse(t_max)).orElse(prevHit)
    }
  }

  final def bounding_box: AABB = objects.map(_.bounding_box).reduce(_ + _)
}
