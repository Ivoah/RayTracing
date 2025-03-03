import scala.util.Random
import scala.math

case class BVH(left: Hittable, right: Hittable) extends Hittable {
  val bounding_box: AABB = left.bounding_box + right.bounding_box

  def hit(r: Ray, t_min: Double, t_max: Double): Option[Hit] = {
    if (!bounding_box.hit(r, t_min, t_max)) return None

    val hit_left = left.hit(r, t_min, t_max)
    val hit_right = right.hit(r, t_min, hit_left.map(_.t).getOrElse(t_max))

    hit_right.orElse(hit_left)
  }
}

object BVH {
  def apply(objects: Seq[Hittable]): BVH = {
    val comparator = Random.between(0, 3) match {
      case 0 => (o1: Hittable, o2: Hittable) => o1.bounding_box.v_min.x < o2.bounding_box.v_min.x
      case 1 => (o1: Hittable, o2: Hittable) => o1.bounding_box.v_min.y < o2.bounding_box.v_min.y
      case 2 => (o1: Hittable, o2: Hittable) => o1.bounding_box.v_min.z < o2.bounding_box.v_min.z
    }

    objects
      .sortWith(comparator)
      .grouped(math.ceil(objects.size.toDouble/2).toInt)
      .map(side => if (side.length > 1) BVH(side) else side.head)
      .toSeq match {
      case Seq(only) => BVH(only, only)
      case Seq(left, right) => BVH(left, right)
    }
  }

  def fromSTL(source: String, material: Material): BVH = {
    val triangle_re =
      raw"""(?m)^facet normal (-?\d+.\d+) (-?\d+.\d+) (-?\d+.\d+)
           |outer loop
           |vertex (-?\d+.\d+) (-?\d+.\d+) (-?\d+.\d+)
           |vertex (-?\d+.\d+) (-?\d+.\d+) (-?\d+.\d+)
           |vertex (-?\d+.\d+) (-?\d+.\d+) (-?\d+.\d+)
           |endloop
           |endfacet""".stripMargin.r
    val triangles = triangle_re.findAllMatchIn(source).map { m =>
      Triangle(
        (
          Vec3(m.group(4).toDouble, m.group(5).toDouble, m.group(6).toDouble),
          Vec3(m.group(7).toDouble, m.group(8).toDouble, m.group(9).toDouble),
          Vec3(m.group(10).toDouble, m.group(11).toDouble, m.group(12).toDouble),
        ),
        material
      )
    }.toSeq
    BVH(triangles)
  }
}
