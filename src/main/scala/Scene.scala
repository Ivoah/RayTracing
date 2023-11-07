import play.api.libs.json.Json

import java.io.File
import java.nio.file.Files

case class Scene(camera: Camera, world: Hittable)

object Scene {
  def fromFile(scene: File, dump: Boolean = false): Option[Scene] = {
    try {
      val json = Json.parse(Files.readString(scene.toPath))
      System.setProperty("user.dir", scene.getAbsoluteFile.getParent)

      import JsonFormats.*
      val camera = (json \ "camera").as[Camera]
      implicit val materials: Map[String, Material] = (json \ "materials").as[Map[String, Material]]
      val world = BVH((json \ "world").as[Seq[Hittable]]: _*)
      if (dump) pprint.pprintln(world)
      Some(Scene(camera, world))
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        None
    }
  }
}
