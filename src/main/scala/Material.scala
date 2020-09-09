import scala.math._
import scala.util.Random

trait Material {
  def scatter(ray: Ray, hit: Hit): Option[(Ray, Vec3)]
}

case class Diffuse(texture: Texture) extends Material {
  def scatter(ray: Ray, hit: Hit): Option[(Ray, Vec3)] = {
    Some((Ray(hit.position, hit.normal + Vec3.random_unit_vector), texture(hit.uv, hit.position)))
  }
}

case class Glossy(texture: Texture, roughness: Double) extends Material {
  def scatter(ray: Ray, hit: Hit): Option[(Ray, Vec3)] = {
    val scattered = Ray(hit.position, ray.direction.unit_vector.reflect(hit.normal) + roughness*Vec3.random_in_unit_sphere)
    if (scattered.direction.dot(hit.normal) > 0) Some((scattered, texture(hit.uv, hit.position)))
    else None
  }
}

case class Glass(texture: Texture, ior: Double) extends Material {
  private def schlick(cosine: Double, ref_idx: Double) = {
    val r0 = (1 - ref_idx)/(1 + ref_idx)
    val r1 = r0*r0
    r1 + (1 - r1)*pow(1 - cosine, 5)
  }

  private def refract(uv: Vec3, normal: Vec3, etai_over_etat: Double) = {
    val cos_theta = -uv.dot(normal)
    val r_out_perp = etai_over_etat*(uv + cos_theta*normal)
    val r_out_parallel = -sqrt(abs(1.0 - r_out_perp.length_squared))*normal
    r_out_perp + r_out_parallel
  }

  def scatter(ray: Ray, hit: Hit): Option[(Ray, Vec3)] = {
    val etai_over_etat = if (hit.front_face) 1.0/ior else ior
    val cos_theta = min(-ray.direction.unit_vector.dot(hit.normal), 1.0)
    val sin_theta = sqrt(1.0 - cos_theta*cos_theta)

    if (etai_over_etat*sin_theta > 1 || schlick(cos_theta, etai_over_etat) > Random.nextDouble()) {
      val reflected = ray.direction.unit_vector.reflect(hit.normal)
      Some((Ray(hit.position, reflected), texture(hit.uv, hit.position)))
    } else {
      val refracted = refract(ray.direction.unit_vector, hit.normal, etai_over_etat)
      Some((Ray(hit.position, refracted), texture(hit.uv, hit.position)))
    }
  }
}
