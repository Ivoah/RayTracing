import scala.util.Random
import org.scalajs.dom._
import org.scalajs.dom.ext.Ajax
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import play.api.libs.json._

object RayTracing extends App {

  case class Options(
    filename: Option[String] = None,
    width: Int = 400,
    height: Int = 225,
    samples: Int = 8,
    scene: Option[String] = None
  )

  var options = Options()

  def loadScene(scene: String) = {
    try {
      val json = Json.parse(scene)

      import JsonReads._
      val camera = (json \ "camera").as[Camera]
      implicit val materials: Map[String, Material] = (json \ "materials").as[Map[String, Material]]
      val world = BVH((json \ "world").as[Seq[Hittable]]: _*)
      Some((camera, world))
    } catch {
      case _: Throwable => None
    }
  }

  def render(camera: Camera, world: Hittable, update: Option[Int => Unit] = None, finish: Option[Double => Unit] = None): Unit = {
    val canvas = document.getElementById("img").asInstanceOf[html.Canvas]
    val ctx = canvas.getContext("2d")
    val img = ctx.createImageData(1, 1).asInstanceOf[raw.ImageData]
    img.data(3) = 255
    val start = System.currentTimeMillis()

    def renderLine(j: Int = 0): Unit = {
      for (i <- 0 until options.width) {
        val (r, g, b) = ((for (_ <- 0 until options.samples) yield {
          val u = (i + Random.nextDouble())/(options.width - 1)
          val v = (j + Random.nextDouble())/(options.height - 1)
          camera.ray_color(u, v, world)
        }).reduce(_ + _) / options.samples).toRGB
        img.data(0) = r
        img.data(1) = g
        img.data(2) = b

        ctx.putImageData(img, i, options.height - j - 1)
      }
      update.foreach(_ (j))
      if (j + 1 < options.height) window.requestAnimationFrame(_ => renderLine(j + 1))
      else {
        val stop = System.currentTimeMillis()
        finish.foreach(_((stop - start)/1000.0))
      }
    }
    window.requestAnimationFrame(_ => renderLine())
  }

  val button = document.getElementById("render").asInstanceOf[html.Input]
  button.onclick = e => {
    val scenes = document.getElementById("scenes").asInstanceOf[html.Select]
    val samples = document.getElementById("samples").asInstanceOf[html.Input]
    options = options.copy(samples = samples.valueAsNumber.toInt)
    Ajax.get(scenes.value).foreach { xhr =>
      loadScene(xhr.responseText) match {
        case Some((camera, world)) =>
          render(camera, world,
            Some(line => println(s"Rendered line [${line + 1}/${options.height}]")),
            Some(time => println(s"Rendered ${options.height} lines in $time seconds"))
          )
        case None =>
          println(s"Error loading scene")
      }
    }
  }
}
