import org.scalajs.dom.fetch
import org.scalajs.dom
import scala.scalajs.js.Thenable.Implicits.*
//import scala.concurrent.ExecutionContext.Implicits.global
import scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.Try

import scala.concurrent.*
import scala.concurrent.duration.*

import scalajs.js

given FileLoader = new FileLoader {
  def loadImage(path: String): Image = {
    new Image(0, 0, IndexedSeq()) {
      val img = dom.Image()
      var imgPixels: Option[IndexedSeq[Vec3]] = None
      override def apply(uv: Vec2, p: Vec3): Vec3 = {
        imgPixels.map { p =>
          val u = Util.clamp(uv.x, 0.0, 1.0)
          val v = 1.0 - Util.clamp(uv.y, 0.0, 1.0)  // Flip V to image coordinates

          // Clamp integer mapping, since actual coordinates should be less than 1.0
          val i = Util.clamp((u * img.width).toInt, 0, img.width - 1)
          val j = Util.clamp((v * img.height).toInt, 0, img.height - 1)

          p(j*img.width + i)
        }.getOrElse(Vec3(0, 0, 0))
      }

      img.src = s"scenes/$path"
      println(s"Loading $path")
      img.onload = { _ =>
        val canvas = dom.document.createElement("canvas").asInstanceOf[dom.HTMLCanvasElement]
        canvas.width = img.width
        canvas.height = img.height
        val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
        ctx.drawImage(img, 0, 0)
        imgPixels = Some(ctx.getImageData(0, 0, img.width, img.height)
          .data
          .grouped(4)
          .map(p => Vec3(p(0)/255.0, p(1)/255.0, p(2)/255.0))
          .toIndexedSeq)
        println(s"Loaded $path")
      }
    }
  }

  def loadFile(path: String): String = ???
}
