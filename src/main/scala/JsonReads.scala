import play.api.libs.json._
import play.api.libs.functional.syntax._

object JsonReads {
  implicit val vec3Reads: Reads[Vec3] = (
    (JsPath \ 0).read[Double] and
      (JsPath \ 1).read[Double] and
      (JsPath \ 2).read[Double]
    )(Vec3.apply _)

  implicit val cameraReads: Reads[Camera] = Json.reads[Camera]

  implicit val diffuseReads: Reads[Diffuse] = Json.reads[Diffuse]
  implicit val glossyReads: Reads[Glossy] = Json.reads[Glossy]
  implicit val glassReads: Reads[Glass] = Json.reads[Glass]
  implicit val materialRead: Reads[(String, Material)] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "type").read[String] and
      (JsPath \ "material").read[JsObject]
    ) { (name: String, _type: String, material: JsObject) =>
    (name, _type match {
      case "Diffuse" => material.as[Diffuse]
      case "Glass" => material.as[Glass]
      case "Glossy" => material.as[Glossy]
    })
  }

  def sphereReads(materials: Map[String, Material]): Reads[Sphere] = (
    (JsPath \ "center").read[Vec3] and
      (JsPath \ "radius").read[Double] and
      (JsPath \ "material").read[String]
    ) { (center: Vec3, radius: Double, material: String) =>
    Sphere(center, radius, materials(material))
  }
  def hittableRead(materials: Map[String, Material]): Reads[Hittable] = (
    (JsPath \ "type").read[String] and
      (JsPath \ "obj").read[JsObject]
    ) { (_type: String, obj: JsObject) =>
    _type match {
      case "Sphere" => obj.as[Sphere](sphereReads(materials))
    }
  }

}
