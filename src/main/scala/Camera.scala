import scala.math._

case class Camera(origin: Vec3, target: Vec3, vup: Vec3, vfov: Double,
                  aspect_ratio: Double, aperture: Double, focus_distance: Double) {
  private val viewport_height = 2.0*tan(vfov.toRadians/2)
  private val viewport_width = aspect_ratio*viewport_height

  private val w = (origin - target).unit_vector
  private val u = vup.cross(w).unit_vector
  private val v = w.cross(u)

  private val focus = if (focus_distance < 0) (target - origin).length else focus_distance
  private val horizontal = focus*viewport_width*u
  private val vertical = focus*viewport_height*v
  private val lower_left_corner = origin - horizontal/2 - vertical/2 - focus*w

  private val lens_radius = aperture/2

  def get_ray(s: Double, t: Double): Ray = {
    val rd = lens_radius*Vec3.random_in_unit_disk
    val offset = u*rd.x + v*rd.y
    Ray(origin + offset, lower_left_corner + s*horizontal + t*vertical - origin - offset)
  }

  def ray_color(u: Double, v: Double, world: Hittable, depth: Int = 50): Vec3 = ray_color(get_ray(u, v), world, depth)
  def ray_color(ray: Ray, world: Hittable, depth: Int): Vec3 = {
    if (depth <= 0) return Vec3(0, 0, 0)
    world.hit(ray, 0.001, Double.PositiveInfinity) match {
      case Some(hit) => hit.material.scatter(ray, hit) match {
          case Some((scattered, attenuation)) =>
            attenuation*ray_color(scattered, world, depth - 1)
          case None => Vec3(0, 0, 0)
        }
      case None =>
        val unit_direction = ray.direction.unit_vector
        val t = 0.5*(unit_direction.y + 1.0)
        (1.0 - t)*Vec3(1.0, 1.0, 1.0) + t*Vec3(0.5, 0.7, 1.0)
    }
  }
}
