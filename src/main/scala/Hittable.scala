case class Hit(ray: Ray, outward_normal: Vec3, t: Double, obj: Hittable) {
    def position = ray.at(t)
    def front_face = ray.direction.dot(outward_normal) < 0
    def normal = if (front_face) outward_normal else -outward_normal
}

abstract class Hittable(val mat: Material) {
    def hit(r: Ray, t_min: Double, t_max: Double): Option[Hit]
}

case class HittableList(objects: Hittable*) {
    def hit(r: Ray, t_min: Double, t_max: Double) = {
        objects.foldLeft[Option[Hit]](None) { (prevHit: Option[Hit], obj: Hittable) =>
            obj.hit(r, t_min, prevHit.map(_.t).getOrElse(t_max)).orElse(prevHit)
        }
    }
}
