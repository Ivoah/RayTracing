import scala.math._

trait Material {
    def scatter(ray: Ray, hit: Hit): Option[(Ray, Vec3)]
}

case class Diffuse(color: Vec3) extends Material {
    def scatter(ray: Ray, hit: Hit) = {
        Some((Ray(hit.position, hit.normal + Vec3.random_unit_vector), color))
    }
}

case class Glossy(color: Vec3, roughness: Double) extends Material {
    def scatter(ray: Ray, hit: Hit) = {
        val scattered = Ray(hit.position, ray.direction.unit_vector.reflect(hit.normal) + roughness*Vec3.random_in_unit_sphere)
        if (scattered.direction.dot(hit.normal) > 0) Some((scattered, color))
        else None
    }
}

case class Glass(color: Vec3, ior: Double) extends Material {
    val rand = new scala.util.Random

    def schlick(cosine: Double, ref_idx: Double) = {
        val r0 = (1 - ref_idx)/(1 + ref_idx)
        val r1 = r0*r0
        r1 + (1 - r1)*pow((1 - cosine), 5)
    }

    def refract(uv: Vec3, normal: Vec3, etai_over_etat: Double) = {
        val cos_theta = -uv.dot(normal)
        val r_out_perp = etai_over_etat*(uv + cos_theta*normal)
        val r_out_parallel = -sqrt(abs(1.0 - r_out_perp.length_squared))*normal
        r_out_perp + r_out_parallel
    }

    def scatter(ray: Ray, hit: Hit) = {
        val etai_over_etat = if (hit.front_face) 1.0/ior else ior
        val cos_theta = min(-ray.direction.unit_vector.dot(hit.normal), 1.0)
        val sin_theta = sqrt(1.0 - cos_theta*cos_theta)

        if (etai_over_etat*sin_theta > 1 || schlick(cos_theta, etai_over_etat) > rand.nextDouble()) {
            val reflected = ray.direction.unit_vector.reflect(hit.normal)
            Some((Ray(hit.position, reflected), color))
        } else {
            val refracted = refract(ray.direction.unit_vector, hit.normal, etai_over_etat)
            Some((Ray(hit.position, refracted), color))
        }
    }
}
