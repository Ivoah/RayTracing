import scala.math._

case class Camera(origin: Vec3, lookat: Vec3, vup: Vec3, vfov: Double,
                  aspect_ratio: Double, aperture: Double, focus_distance: Double) {
    private val viewport_height = 2.0*tan(vfov.toRadians/2)
    private val viewport_width = aspect_ratio*viewport_height

    private val w = (origin - lookat).unit_vector
    private val u = vup.cross(w).unit_vector
    private val v = w.cross(u)

    private val horizontal = focus_distance*viewport_width*u
    private val vertical = focus_distance*viewport_height*v
    private val lower_left_corner = origin - horizontal/2 - vertical/2 - focus_distance*w
    
    private val lens_radius = aperture/2
    
    def get_ray(s: Double, t: Double): Ray = {
        val rd = lens_radius*Vec3.random_in_unit_disk
        val offset = u*rd.x + v*rd.y

        Ray(
            origin + offset,
            lower_left_corner + s*horizontal + t*vertical - origin - offset
        )
    }

    def ray_color(u: Double, v: Double, world: HittableList, depth: Int): Vec3 = ray_color(get_ray(u, v), world, depth)
    def ray_color(ray: Ray, world: HittableList, depth: Int): Vec3 = {
        if (depth <= 0) return Vec3(0, 0, 0)
        world.hit(ray, 0.001, Double.PositiveInfinity) match {
            case Some(hit) =>
                hit.obj.mat.scatter(ray, hit) match {
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
