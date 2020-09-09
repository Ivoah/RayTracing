import scala.util.Random
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.swing._
import scala.swing.Swing._

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import play.api.libs.json._

object RayTracing extends App {

  case class Options(
                      filename: Option[String] = None,
                      width: Option[Int] = Some(400),
                      height: Option[Int] = Some(225),
                      samples: Option[Int] = Some(32),
                      scene: Option[String] = Some("scene.json")
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
  val image_width = options.width.get
  val image_height = options.height.get
  val aspect_ratio = options.width.get.toDouble/options.height.get
  val samples = options.samples.get
  val max_depth = 50

  val sceneFile = io.Source.fromFile(options.scene.get)
  val json = Json.parse(sceneFile.getLines().mkString)
  sceneFile.close()

  import JsonReads._
  val camera = (json \ "camera").as[Camera]
  implicit val materials: Map[String, Material] = (json \ "materials").as[Map[String, Material]]
  val world = BVH((json \ "world").as[Seq[Hittable]]: _*)

  val rand = new scala.util.Random

  val img = new BufferedImage(image_width, image_height, BufferedImage.TYPE_INT_RGB)

  val frame: Frame = new MainFrame {
    title = "Scala ray tracer"
    resizable = false

    menuBar = new MenuBar {
      contents += new Menu ("File") {
        contents += new MenuItem(Action("Save") {
          val chooser = new FileChooser
          chooser.showSaveDialog(frame)
          ImageIO.write(img, "png", chooser.selectedFile)
        })
      }
    }

    contents = new Panel {
      preferredSize = (image_width, image_height)
      override def paintComponent(g: Graphics2D): Unit = {
        g.drawImage(img, 0, 0, null)
      }
    }

    pack()
    centerOnScreen()
    open()
  }

  val start = System.currentTimeMillis()
  for (j <- 0 until image_height) {
    for (i <- 0 until image_width) {
      val pixel = Await.result(Future.reduceLeft(for (_ <- 0 until samples) yield Future {
        val u = (i + Random.nextDouble())/(image_width - 1)
        val v = (j + Random.nextDouble())/(image_height - 1)
        camera.ray_color(u, v, world, max_depth)
      })(_ + _), Duration.Inf)/samples

      img.setRGB(i, image_height - j - 1, pixel.toRGB)
      frame.repaint()
    }
  }
  val stop = System.currentTimeMillis()
  Dialog.showMessage(frame, s"Rendered ${image_height} lines in ${(stop - start)/1000.0} seconds")

  options.filename.map { filename =>
    ImageIO.write(img, "png", new File(filename))
  }
}
