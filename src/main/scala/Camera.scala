import java.awt.image.BufferedImage
import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.math.*
import scala.util.{Random, boundary}

case class Camera(origin: Vec3, target: Vec3, vup: Vec3, vfov: Double,
                  aspect_ratio: Double, aperture: Double, focus_distance: Double, background: Vec3) {
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
    if (depth <= 0) return 0
    world.hit(ray, 0.001, Double.PositiveInfinity) match {
      case Some(hit) => hit.material.scatter(ray, hit) match {
          case Some((scattered, attenuation)) =>
            hit.material.emit(hit.uv, hit.position) + attenuation*ray_color(scattered, world, depth - 1)
          case None => hit.material.emit(hit.uv, hit.position)
        }
      case None => background
    }
  }

  def render(world: Hittable, img: BufferedImage, samples: Int,
             update: Int => Unit = _ => (),
             finish: Double => Unit = _ => (),
             break: () => Boolean = () => false): Unit = {
    val start = System.currentTimeMillis()
    boundary {
      for (j <- 0 until img.getHeight) {
        if (break()) boundary.break()
        for (i <- 0 until img.getWidth) {
          val pixel = Await.result(Future.reduceLeft(for (_ <- 0 until samples) yield Future {
            val u = (i + Random.nextDouble()) / (img.getWidth - 1)
            val v = (j + Random.nextDouble()) / (img.getHeight - 1)
            ray_color(u, v, world)
          })(_ + _), Duration.Inf) / samples

          img.setRGB(i, img.getHeight - j - 1, pixel.toRGB)
        }
        update(j)
      }
    }
    val stop = System.currentTimeMillis()
    finish((stop - start) / 1000.0)
  }
}
