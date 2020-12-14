import play.api.libs.json._
import play.api.libs.functional.syntax._

object JsonReads {
  implicit val vec2Reads: Reads[Vec2] = (JsPath(0).read[Double] and JsPath(1).read[Double])(Vec2.apply _)
  implicit val vec3Reads: Reads[Vec3] = (JsPath(0).read[Double] and JsPath(1).read[Double] and JsPath(2).read[Double])(Vec3.apply _)

  implicit val cameraReads: Reads[Camera] = Json.reads[Camera]

  implicit val solidColorReads: Reads[SolidColor] = Json.reads[SolidColor]
  lazy implicit val checkerReads: Reads[Checker] = Json.reads[Checker]
  implicit val perlinReads: Reads[Perlin] = Json.reads[Perlin]
  implicit val textureReads: Reads[Texture] = (
    (JsPath \ "type").read[String] and JsPath.read[JsObject]
  ) { (_type: String, texture: JsObject) =>
    _type match {
      case "SolidColor" => texture.as[SolidColor]
      case "Checker" => texture.as[Checker]
      case "Perlin" => texture.as[Perlin]
      case "Image" => Checker(5, SolidColor(Vec3(1, 0, 1)), SolidColor(Vec3(0, 0, 0)))
    }
  }

  implicit val diffuseReads: Reads[Diffuse] = Json.reads[Diffuse]
  implicit val glossyReads: Reads[Glossy] = Json.reads[Glossy]
  implicit val glassReads: Reads[Glass] = Json.reads[Glass]
  implicit val emissionReads: Reads[Emission] = Json.reads[Emission]
  implicit val materialReads: Reads[Material] = (
    (JsPath \ "type").read[String] and
    JsPath.read[JsObject]
  ) { (_type: String, material: JsObject) =>
    _type match {
      case "Diffuse" => material.as[Diffuse]
      case "Glass" => material.as[Glass]
      case "Glossy" => material.as[Glossy]
      case "Emission" => material.as[Emission]
    }
  }

  implicit def sphereReads(implicit materials: Map[String, Material]): Reads[Sphere] = (
    (JsPath \ "center").read[Vec3] and
      (JsPath \ "radius").read[Double] and
      (JsPath \ "material").read[String]
  ) { (center: Vec3, radius: Double, material: String) =>
    Sphere(center, radius, materials(material))
  }
  implicit def xyRectReads(implicit materials: Map[String, Material]): Reads[XYRect] = (
    (JsPath \ "sides").read[Seq[Double]] and
      (JsPath \ "z").read[Double] and
      (JsPath \ "material").read[String]
    ) { (sides: Seq[Double], z: Double, material: String) =>
    XYRect(sides(0), sides(1), sides(2), sides(3), z, materials(material))
  }
  implicit def xzRectReads(implicit materials: Map[String, Material]): Reads[XZRect] = (
    (JsPath \ "sides").read[Seq[Double]] and
      (JsPath \ "y").read[Double] and
      (JsPath \ "material").read[String]
    ) { (sides: Seq[Double], y: Double, material: String) =>
    XZRect(sides(0), sides(1), sides(2), sides(3), y, materials(material))
  }
  implicit def yzRectReads(implicit materials: Map[String, Material]): Reads[YZRect] = (
    (JsPath \ "sides").read[Seq[Double]] and
      (JsPath \ "x").read[Double] and
      (JsPath \ "material").read[String]
    ) { (sides: Seq[Double], x: Double, material: String) =>
    YZRect(sides(0), sides(1), sides(2), sides(3), x, materials(material))
  }
  implicit def hittableRead(implicit materials: Map[String, Material]): Reads[Hittable] = (
    (JsPath \ "type").read[String] and
    JsPath.read[JsObject]
  ) { (_type: String, obj: JsObject) =>
    _type match {
      case "Sphere" => obj.as[Sphere]
      case "XYRect" => obj.as[XYRect]
      case "XZRect" => obj.as[XZRect]
      case "YZRect" => obj.as[YZRect]
    }
  }
}
