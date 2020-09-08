import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import play.api.libs.json._

object RayTracing extends App {

  case class Options(
                      filename: Option[String] = Some("img.ppm"),
                      width: Option[Int] = Some(400),
                      height: Option[Int] = Some(225),
                      samples: Option[Int] = Some(32),
                      scene: Option[String] = None
                    )

  @annotation.tailrec
  def parseOptions(args: List[String], options: Option[Options] = Some(Options())): Option[Options] = {
    args match {
      case "--filename" :: filename :: tail => parseOptions(tail, options.map(_.copy(filename = Some(filename))))
      case "-o" :: filename :: tail => parseOptions(tail, options.map(_.copy(filename = Some(filename))))
      case "--width" :: width :: tail => parseOptions(tail, options.map(_.copy(width = width.toIntOption)))
      case "-w" :: width :: tail => parseOptions(tail, options.map(_.copy(width = width.toIntOption)))
      case "--height" :: height :: tail => parseOptions(tail, options.map(_.copy(height = height.toIntOption)))
      case "-h" :: height :: tail => parseOptions(tail, options.map(_.copy(height = height.toIntOption)))
      case "--samples" :: samples :: tail => parseOptions(tail, options.map(_.copy(samples = samples.toIntOption)))
      case "-s" :: samples :: tail => parseOptions(tail, options.map(_.copy(samples = samples.toIntOption)))
      case "--scene" :: scene :: tail => parseOptions(tail, options.map(_.copy(scene = Some(scene))))
      case Nil => options
      case _ => None
    }
  }

  var options = Options()
  try {
    options = parseOptions(args.toList).get
  } catch {
    case _: NoSuchElementException =>
      println("Error parsing arguments")
      System.exit(1)
  }

  // Image
  val filename = options.filename.get
  val image_width = options.width.get
  val image_height = options.height.get
  val aspect_ratio = options.width.get.toDouble/options.height.get
  val samples = options.samples.get
  val max_depth = 50

  val sceneFile = io.Source.fromFile(options.scene.get)
  val json = Json.parse(sceneFile.getLines.mkString)
  sceneFile.close()

  import JsonReads._
  val camera = (json \ "camera").as[Camera]
  val materials = (json \ "materials").as[JsArray].value.map(_.as[(String, Material)]).toMap
  val world = BVH((json \ "world").as[JsArray].value.map(_.as[Hittable](JsonReads.hittableRead(materials))).toSeq: _*)

  val rand = new scala.util.Random

  val ppm = new java.io.PrintWriter(filename)
  ppm.write(s"P3\n$image_width $image_height\n255\n")

  for (j <- image_height - 1 to 0 by -1) {
    print(s"\rRendering line [${image_height - j}/$image_height]")
    for (i <- 0 until image_width) {
      val pixel = Await.result(Future.reduceLeft(for (_ <- 0 until samples) yield Future {
        val u = (i + rand.nextDouble())/(image_width - 1)
        val v = (j + rand.nextDouble())/(image_height - 1)
        camera.ray_color(u, v, world, max_depth)
      })(_ + _), Duration.Inf)/samples

      ppm.write(pixel.toPPM)
    }
  }
  println()

  ppm.close()
}
