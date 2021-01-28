import scala.util.Random

object BVH {
  def apply(objects: Hittable*): BVH = {
    val comparator = Random.between(0, 3) match {
      case 0 => (o1: Hittable, o2: Hittable) => o1.bounding_box.v_min.x < o2.bounding_box.v_min.x
      case 1 => (o1: Hittable, o2: Hittable) => o1.bounding_box.v_min.y < o2.bounding_box.v_min.y
      case 2 => (o1: Hittable, o2: Hittable) => o1.bounding_box.v_min.z < o2.bounding_box.v_min.z
    }
    objects match {
      case obj :: Nil => BVH(obj, obj)
      case obj1 :: obj2 :: Nil =>
        if (comparator(obj1, obj2)) BVH(obj1, obj2)
        else BVH(obj2, obj1)
      case objs =>
        val sorted = objs.sortWith(comparator)
        val (left, right) = sorted.zipWithIndex.partition(_._2 > sorted.size/2)
        BVH(
          BVH(left.map(_._1): _*),
          BVH(right.map(_._1): _*)
        )
    }
  }
}

case class BVH(left: Hittable, right: Hittable) extends Hittable {
  def hit(r: Ray, t_min: Double, t_max: Double): Option[Hit] = {
    if (!bounding_box.hit(r, t_min, t_max)) return None

    val hit_left = left.hit(r, t_min, t_max)
    val hit_right = right.hit(r, t_min, hit_left.map(_.t).getOrElse(t_max))

    hit_right.orElse(hit_left)
  }

  val bounding_box: AABB = left.bounding_box + right.bounding_box
}
