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
    width: Int = 400,
    height: Int = 225,
    samples: Int = 32,
    scene: String = "scene.json"
  )

  @annotation.tailrec
  def parseOptions(args: List[String], options: Option[Options] = Some(Options())): Option[Options] = {
    args match {
      case "--filename" :: filename :: tail => parseOptions(tail, options.map(_.copy(filename = Some(filename))))
      case "-o" :: filename :: tail => parseOptions(tail, options.map(_.copy(filename = Some(filename))))
      case "--width" :: width :: tail => parseOptions(tail, options.map(_.copy(width = width.toInt)))
      case "-w" :: width :: tail => parseOptions(tail, options.map(_.copy(width = width.toInt)))
      case "--height" :: height :: tail => parseOptions(tail, options.map(_.copy(height = height.toInt)))
      case "-h" :: height :: tail => parseOptions(tail, options.map(_.copy(height = height.toInt)))
      case "--samples" :: samples :: tail => parseOptions(tail, options.map(_.copy(samples = samples.toInt)))
      case "-s" :: samples :: tail => parseOptions(tail, options.map(_.copy(samples = samples.toInt)))
      case "--scene" :: scene :: tail => parseOptions(tail, options.map(_.copy(scene = scene)))
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

  def loadScene(scene: File) = {
    val sceneFile = io.Source.fromFile(scene)
    val json = Json.parse(sceneFile.getLines().mkString)
    sceneFile.close()

    import JsonReads._
    val camera = (json \ "camera").as[Camera]
    implicit val materials: Map[String, Material] = (json \ "materials").as[Map[String, Material]]
    val world = BVH((json \ "world").as[Seq[Hittable]]: _*)
    (camera, world)
  }

  val img = new BufferedImage(options.width, options.height, BufferedImage.TYPE_INT_RGB)

  options.filename match {
    case Some(filename) =>
      val (camera, world) = loadScene(new File(options.scene))
      render(camera, world,
        Some(line => print(s"\rRendered line [${line + 1}/${options.height}]")),
        Some(time => println(s"\nRendered ${options.height} lines in $time seconds"))
      )
      ImageIO.write(img, "png", new File(filename))
    case None =>
      var (camera, world) = loadScene(new File(options.scene))
      lazy val frame: Frame = new MainFrame {
        title = s"Scala ray tracer: ${options.scene}"
        resizable = false

        menuBar = new MenuBar {
          contents ++= Seq(
              new Menu("File") {
              contents ++= Seq(
                new MenuItem(Action("Load scene") {
                  val chooser = new FileChooser
                  if (chooser.showOpenDialog(frame) == FileChooser.Result.Approve) {
                    frame.title = s"Scala ray tracer: ${chooser.selectedFile.getName}"
                    loadScene(chooser.selectedFile) match {
                      case (new_camera, new_world) =>
                        camera = new_camera
                        world = new_world
                    }
                  }
                }),
                new MenuItem(Action("Save") {
                  val chooser = new FileChooser
                  if (chooser.showSaveDialog(frame) == FileChooser.Result.Approve)
                    ImageIO.write(img, "png", chooser.selectedFile)
                })
              )
            },
            new MenuItem(Action("Render") {
              new Thread {
                override def run() {
                  render(camera, world,
                    Some(_ => frame.repaint()),
                    Some(time => Dialog.showMessage(frame, s"Rendered ${options.height} lines in $time seconds"))
                  )
                }
              }.start()
            })
          )
        }

        contents = new Panel {
          preferredSize = (options.width, options.height)

          override def paintComponent(g: Graphics2D): Unit = {
            g.drawImage(img, 0, 0, null)
          }
        }

        pack()
        centerOnScreen()
        open()
      }

      render(camera, world,
        Some(_ => frame.repaint()),
        Some(time => Dialog.showMessage(frame, s"Rendered ${options.height} lines in $time seconds"))
      )
  }

  def render(camera: Camera, world: Hittable, update: Option[Int => Unit] = None, finish: Option[Double => Unit] = None) {
    val start = System.currentTimeMillis()
    for (j <- 0 until options.height) {
      for (i <- 0 until options.width) {
        val pixel = Await.result(Future.reduceLeft(for (_ <- 0 until options.samples) yield Future {
          val u = (i + Random.nextDouble())/(options.width - 1)
          val v = (j + Random.nextDouble())/(options.height - 1)
          camera.ray_color(u, v, world)
        })(_ + _), Duration.Inf)/options.samples

        img.setRGB(i, options.height - j - 1, pixel.toRGB)
        update.foreach(_(j))
      }
    }
    val stop = System.currentTimeMillis()
    finish.foreach(_((stop - start)/1000.0))
  }
}
