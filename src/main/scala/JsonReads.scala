import play.api.libs.json._
import play.api.libs.functional.syntax._

object JsonReads {
  implicit val vec3Reads: Reads[Vec3] = (
    JsPath(0).read[Double] and
    JsPath(1).read[Double] and
    JsPath(2).read[Double]
  )(Vec3.apply: (Double, Double, Double) => Vec3)

  implicit val cameraReads: Reads[Camera] = Json.reads[Camera]

  implicit val solidColorReads: Reads[SolidColor] = Json.reads[SolidColor]
  lazy implicit val checkerReads: Reads[Checker] = Json.reads[Checker]
  implicit val perlinReads: Reads[Perlin] = Json.reads[Perlin]
  implicit val textureReads: Reads[Texture] = (
    (JsPath \ "type").read[String] and
    JsPath.read[JsObject]
  ) { (_type: String, texture: JsObject) =>
    _type match {
      case "SolidColor" => texture.as[SolidColor]
      case "Checker" => texture.as[Checker]
      case "Perlin" => texture.as[Perlin]
    }
  }

  implicit val diffuseReads: Reads[Diffuse] = Json.reads[Diffuse]
  implicit val glossyReads: Reads[Glossy] = Json.reads[Glossy]
  implicit val glassReads: Reads[Glass] = Json.reads[Glass]
  implicit val materialReads: Reads[Material] = (
    (JsPath \ "type").read[String] and
    JsPath.read[JsObject]
  ) { (_type: String, material: JsObject) =>
    _type match {
      case "Diffuse" => material.as[Diffuse]
      case "Glass" => material.as[Glass]
      case "Glossy" => material.as[Glossy]
    }
  }

  implicit def sphereReads(implicit materials: Map[String, Material]): Reads[Sphere] = (
    (JsPath \ "center").read[Vec3] and
    (JsPath \ "radius").read[Double] and
    (JsPath \ "material").read[String]
  ) { (center: Vec3, radius: Double, material: String) =>
    Sphere(center, radius, materials(material))
  }
  implicit def hittableRead(implicit materials: Map[String, Material]): Reads[Hittable] = (
    (JsPath \ "type").read[String] and
    JsPath.read[JsObject]
  ) { (_type: String, obj: JsObject) =>
    _type match {
      case "Sphere" => obj.as[Sphere]
    }
  }
}
