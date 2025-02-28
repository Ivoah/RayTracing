import play.api.libs.json._
import play.api.libs.functional.syntax._

object JsonFormats {
  implicit val vec2Reads: Reads[Vec2] = (JsPath(0).read[Double] and JsPath(1).read[Double])(Vec2.apply _)
  implicit val vec2Writes: Writes[Vec2] = (v: Vec2) => JsArray(v.toSeq.map(JsNumber(_)))
  implicit val vec3Reads: Reads[Vec3] = (JsPath(0).read[Double] and JsPath(1).read[Double] and JsPath(2).read[Double])(Vec3.apply _)
  implicit val vec3Writes: Writes[Vec3] = (v: Vec3) => JsArray(v.toSeq.map(JsNumber(_)))

  implicit val cameraFormat: Format[Camera] = Json.format[Camera]

  implicit def imageReads(implicit fileLoader: FileLoader): Reads[Image] =
    (JsPath \ "file").read[String].map(fileLoader.loadImage)

  implicit def solidColorReads: Reads[SolidColor] = Json.reads[SolidColor]
  implicit def checkerReads(implicit fileLoader: FileLoader): Reads[Checker] = Json.reads[Checker]
  implicit def perlinReads: Reads[Perlin] = Json.reads[Perlin]
  // implicit val imageReads: Reads[Image] = Json.reads[Image]
  implicit def textureReads(implicit fileLoader: FileLoader): Reads[Texture] = (
    (JsPath \ "type").read[String]
    and JsPath.read[JsObject]
  ) { (_type: String, texture: JsObject) =>
    _type match {
      case "SolidColor" => texture.as[SolidColor]
      case "Checker" => texture.as[Checker]
      case "Perlin" => texture.as[Perlin]
      case "Image" => texture.as[Image]
    }
  }

  implicit def diffuseReads(implicit fileLoader: FileLoader): Reads[Diffuse] = Json.reads[Diffuse]
  implicit def glossyReads(implicit fileLoader: FileLoader): Reads[Glossy] = Json.reads[Glossy]
  implicit def glassReads(implicit fileLoader: FileLoader): Reads[Glass] = Json.reads[Glass]
  implicit def emissionReads(implicit fileLoader: FileLoader): Reads[Emission] = Json.reads[Emission]
  implicit def materialReads(implicit fileLoader: FileLoader): Reads[Material] = (
    (JsPath \ "type").read[String]
    and JsPath.read[JsObject]
  ) { (_type: String, material: JsObject) =>
    _type match {
      case "Diffuse" => material.as[Diffuse]
      case "Glass" => material.as[Glass]
      case "Glossy" => material.as[Glossy]
      case "Emission" => material.as[Emission]
    }
  }

  implicit def sphereReads(implicit materials: Map[String, Material]): Reads[Sphere] = (
    (JsPath \ "center").read[Vec3]
    and (JsPath \ "radius").read[Double]
    and (JsPath \ "material").read[String]
  ) { (center: Vec3, radius: Double, material: String) =>
    Sphere(center, radius, materials(material))
  }
  implicit def triangleReads(implicit materials: Map[String, Material]): Reads[Triangle] = (
    (JsPath \ "vertices").read[(Vec3, Vec3, Vec3)]
    and (JsPath \ "material").read[String]
  ) { (vertices: (Vec3, Vec3, Vec3), material: String) =>
    Triangle(vertices, materials(material))
  }
  def stlReads(implicit fileLoader: FileLoader, materials: Map[String, Material]): Reads[BVH] = (
    (JsPath \ "file").read[String]
    and (JsPath \ "material").read[String]
  ) { (file: String, material: String) =>
    BVH.fromSTL(fileLoader.loadFile(file), materials(material))
  }
  implicit def xyRectReads(implicit materials: Map[String, Material]): Reads[XYRect] = (
    (JsPath \ "sides").read[Seq[Double]]
    and (JsPath \ "z").read[Double]
    and (JsPath \ "material").read[String]
  ) { (sides: Seq[Double], z: Double, material: String) =>
    XYRect(sides(0), sides(1), sides(2), sides(3), z, materials(material))
  }
  implicit def xzRectReads(implicit materials: Map[String, Material]): Reads[XZRect] = (
    (JsPath \ "sides").read[Seq[Double]]
    and (JsPath \ "y").read[Double]
    and (JsPath \ "material").read[String]
  ) { (sides: Seq[Double], y: Double, material: String) =>
    XZRect(sides(0), sides(1), sides(2), sides(3), y, materials(material))
  }
  implicit def yzRectReads(implicit materials: Map[String, Material]): Reads[YZRect] = (
    (JsPath \ "sides").read[Seq[Double]]
    and (JsPath \ "x").read[Double]
    and (JsPath \ "material").read[String]
  ) { (sides: Seq[Double], x: Double, material: String) =>
    YZRect(sides(0), sides(1), sides(2), sides(3), x, materials(material))
  }
  implicit def hittableRead(implicit fileLoader: FileLoader, materials: Map[String, Material]): Reads[Hittable] = (
    (JsPath \ "type").read[String]
    and JsPath.read[JsObject]
  ) { (_type: String, obj: JsObject) =>
    _type match {
      case "STL" => obj.as[BVH](stlReads)
      case "Triangle" => obj.as[Triangle]
      case "Sphere" => obj.as[Sphere]
      case "XYRect" => obj.as[XYRect]
      case "XZRect" => obj.as[XZRect]
      case "YZRect" => obj.as[YZRect]
    }
  }
}
