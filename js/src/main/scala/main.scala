import scala.util.Random
import org.scalajs.dom.{window, html, document, ImageData}
import org.scalajs.dom.ext.Ajax
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import play.api.libs.json._
import scala.util.{Try, Success, Failure}

@main
def main(): Unit = {
  case class Options(
    filename: Option[String] = None,
    width: Int = 400,
    height: Int = 225,
    samples: Int = 8,
    scene: Option[String] = None
  )

  var options = Options()

  def loadScene(scene: String) = Try {
    val json = Json.parse(scene)

    import JsonFormats._
    val camera = (json \ "camera").as[Camera]
    implicit val materials: Map[String, Material] = (json \ "materials").as[Map[String, Material]]
    val world = BVH((json \ "world").as[Seq[Hittable]])
    (camera, world)
  }

  def render(camera: Camera, world: Hittable, update: Option[Int => Unit] = None, finish: Option[Double => Unit] = None): Unit = {
    val canvas = document.getElementById("img").asInstanceOf[html.Canvas]
    val ctx = canvas.getContext("2d")
    val row = ctx.createImageData(options.width, 1).asInstanceOf[ImageData]
    val start = System.currentTimeMillis()

    def renderLine(j: Int = 0): Unit = {
      for (i <- 0 until options.width) {
        val (r, g, b) = ((for (_ <- 0 until options.samples) yield {
          val u = (i + Random.nextDouble())/(options.width - 1)
          val v = (j + Random.nextDouble())/(options.height - 1)
          camera.ray_color(u, v, world)
        }).reduce(_ + _) / options.samples).toRGB2
        row.data(i*4 + 0) = r
        row.data(i*4 + 1) = g
        row.data(i*4 + 2) = b
        row.data(i*4 + 3) = 255
      }
      ctx.putImageData(row, 0, options.height - j - 1)
      update.foreach(_(j))
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
        case Success((camera, world)) =>
          render(camera, world,
            None, //Some(line => println(s"Rendered line [${line + 1}/${options.height}]")),
            Some(time => println(s"Rendered ${options.height} lines in $time seconds"))
          )
        case Failure(e) =>
          println(s"Error loading scene: $e")
      }
    }
  }
}
