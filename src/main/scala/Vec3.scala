import scala.math._
import scala.language.implicitConversions

object Vec3 {
    implicit def intToVec3(v: Int): Vec3 = Vec3(v, v, v)
    implicit def doubleToVec3(v: Double): Vec3 = Vec3(v, v, v)

    private val rand = new scala.util.Random

    def random: Vec3 = Vec3(rand.nextDouble(), rand.nextDouble(), rand.nextDouble())
    def random(min: Double, max: Double): Vec3 = Vec3(
        rand.between(min, max),
        rand.between(min, max),
        rand.between(min, max)
    )

    @scala.annotation.tailrec
    def random_in_unit_sphere: Vec3 = {
        val v = Vec3.random(-1, 1)
        if (v.length_squared <= 1) v else random_in_unit_sphere
    }

    @scala.annotation.tailrec
    def random_in_unit_disk: Vec3 = {
        val v = Vec3(rand.between(-1.0, 1.0), rand.between(-1.0, 1.0), 0)
        if (v.length_squared <= 1) v else random_in_unit_disk
    }

    def random_unit_vector: Vec3 = {
        val a = rand.between(0, 2*Pi)
        val z = rand.between(-1.0, 1.0)
        val r = sqrt(1 - z*z)
        Vec3(r*cos(a), r*sin(a), z)
    }
}

case class Vec3(x: Double, y: Double, z: Double) {
    def unary_- : Vec3 = Vec3(-x, -y, -z)

    def +(other: Vec3): Vec3 = Vec3(x + other.x, y + other.y, z + other.z)
    def -(other: Vec3): Vec3 = Vec3(x - other.x, y - other.y, z - other.z)

    def *(other: Vec3): Vec3 = Vec3(x*other.x, y*other.y, z*other.z)
    def /(other: Vec3): Vec3 = Vec3(x/other.x, y/other.y, z/other.z)

    def dot(other: Vec3): Double = x*other.x + y*other.y + z*other.z
    def cross(other: Vec3): Vec3 = Vec3(
        y*other.z - z*other.y,
        z*other.x - x*other.z,
        x*other.y - y*other.x
    )
    def reflect(other: Vec3): Vec3 = this - 2*dot(other)*other

    def length_squared: Double = x*x + y*y + z*z
    def length: Double = sqrt(length_squared)
    def unit_vector: Vec3 = this/length

    def toPPM = s"${(255.99*sqrt(x)).toInt} ${(255.99*sqrt(y)).toInt} ${(255.99*sqrt(z)).toInt}\n"
}
