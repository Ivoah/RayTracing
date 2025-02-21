import scala.util.Random
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing.*
import scala.swing.Swing.*
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import javax.swing.filechooser.FileNameExtensionFilter
import play.api.libs.json.*
import scala.reflect.Selectable.reflectiveSelectable
import scala.util.boundary, boundary.break

import java.nio.file.Files

@main
def main(args: String*): Unit = {
  import org.rogach.scallop.*

  class Conf(args: Seq[String]) extends ScallopConf(args) {
    val filename: ScallopOption[String] = opt(short = 'o')
    val width: ScallopOption[Int] = opt(default = Some(400))
    val height: ScallopOption[Int] = opt(default = Some(225))
    val samples: ScallopOption[Int] = opt(default = Some(32))
    val dump: ScallopOption[Boolean] = opt(default = Some(false))
    val scene: ScallopOption[String] = trailArg(required = false)

    dependsOnAll(filename, List(scene))
    verify()
  }

  val options = Conf(args)
  var width = options.width()
  var height = options.height()
  var samples = options.samples()

  def loadScene(scene: File) = {
    try {
      val json = Json.parse(Files.readString(scene.toPath))
      System.setProperty("user.dir", scene.getAbsoluteFile.getParent)

      import JsonFormats._
      val camera = (json \ "camera").as[Camera]
      implicit val materials: Map[String, Material] = (json \ "materials").as[Map[String, Material]]
      val world = BVH((json \ "world").as[Seq[Hittable]])
      if (options.dump()) pprint.pprintln(world)
      Some((camera, world))
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        None
    }
  }

  def formatDuration(t: Double) = {
    if (t >= 3600) f"${t/3600}%02.0fh:${(t%3600)/60}%02.0fm:${t%60}%05.2fs"
    else f"${(t%3600)/60}%02.0fm:${t%60}%05.2fs"
  }

  var img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

  options.filename.toOption match {
    case Some(filename) =>
      options.scene.toOption match {
        case Some(scene) =>
          loadScene(new File(scene)) match {
            case Some((camera, world)) =>
              render(camera, world,
                line => print(s"\rRendered line [${line + 1}/${height}]"),
                time => println(s"\nTime: ${formatDuration(time)}")
              )
              ImageIO.write(img, "png", new File(filename))
            case None =>
              println(s"Error loading scene $scene")
              System.exit(1)
          }
        case None =>
          println("No scene file specified")
          System.exit(1)
      }
    case None =>
      val renderPanel = new Panel {
        preferredSize = (width, height)

        override def paintComponent(g: Graphics2D): Unit = {
          g.drawImage(img, 0, 0, null)
        }
      }

      val statusBar: GridPanel {
        val label: Label
        val progressBar: ProgressBar
        def setProgressBar(): Unit
        def setLabel(): Unit
      } = new GridPanel(1, 1) {
        val label: Label = new Label("") {
          xAlignment = Alignment.Left
        }
        val progressBar: ProgressBar = new ProgressBar {
          min = 0
          max = height
        }

        def setProgressBar(): Unit = contents(0) = progressBar
        def setLabel(): Unit = contents(0) = label

        contents += label
      }

      var scene = options.scene.toOption.flatMap(f => loadScene(new File(f)))

      var thread: Option[RenderThread] = None
      class RenderThread(val bar: MenuBar) extends Thread {
        var rendering = false
        override def run(): Unit = {
          scene match {
            case Some((camera, world)) =>
              bar.contents.foreach(_.enabled = false)
              val renderButton = bar.contents.find(_.name == "Render").get.asInstanceOf[MenuItem]
              renderButton.text = "Stop"
              renderButton.enabled = true
              rendering = true
              statusBar.progressBar.max = height
              statusBar.setProgressBar()
              render(camera, world,
                line => {statusBar.progressBar.value = line; frame.repaint()},
                time => {
                  statusBar.label.text = s"Time ${formatDuration(time)}"
                  statusBar.setLabel()
                },
                () => !rendering
              )
              thread = None
              bar.contents.foreach(_.enabled = true)
              renderButton.text = "Render"
            case None => thread = None
          }
        }

        def break(): Unit = {
          rendering = false
        }
      }

      lazy val frame: Frame = new MainFrame {
        title = options.scene.toOption match {
          case Some(sceneName) => s"Scala ray tracer: $sceneName"
          case None => "Scala ray tracer"
        }
        resizable = false

        menuBar = new MenuBar {
          contents ++= Seq(
            new Menu("File") {
              contents ++= Seq(
                new MenuItem(Action("Load scene") {
                  val chooser = new FileChooser(new File("."))
                  chooser.fileFilter = new FileNameExtensionFilter("Scene files", "json")
                  if (chooser.showOpenDialog(frame) == FileChooser.Result.Approve) {
                    scene = loadScene(chooser.selectedFile)
                    if (scene.isDefined) {
                      frame.title = s"Scala ray tracer: ${chooser.selectedFile.getName}"
                    } else {
                      Dialog.showMessage(frame, s"Error loading scene ${chooser.selectedFile.getName}", "Error", Dialog.Message.Error)
                    }
                  }
                }),
                new MenuItem(Action("Save image") {
                  val chooser = new FileChooser(new File("."))
                  if (chooser.showSaveDialog(frame) == FileChooser.Result.Approve)
                    ImageIO.write(img, "png", chooser.selectedFile)
                })
              )
            },
            new Menu("Options") {
              contents ++= Seq(
                new MenuItem(Action("Width") {
                  Dialog.showInput(frame, "Image width", initial = width.toString).foreach { str =>
                    str.toIntOption match {
                      case Some(newWidth) =>
                        width = newWidth
                        renderPanel.preferredSize = (width, height)
                        renderPanel.revalidate()
                        frame.pack()
                        img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
                      case None => Dialog.showMessage(frame, s"""""$str" is not a number""", "Error", Dialog.Message.Error)
                    }
                  }
                }),
                new MenuItem(Action("Height") {
                  Dialog.showInput(frame, "Image height", initial = height.toString).foreach { str =>
                    str.toIntOption match {
                      case Some(newHeight) =>
                        height = newHeight
                        renderPanel.preferredSize = (width, height)
                        renderPanel.revalidate()
                        frame.pack()
                        img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
                      case None => Dialog.showMessage(frame, s"""""$str" is not a number""", "Error", Dialog.Message.Error)
                    }
                  }
                }),
                new MenuItem(Action("Samples") {
                  Dialog.showInput(frame, "Render samples", initial = samples.toString).foreach { str =>
                    str.toIntOption match {
                      case Some(newSamples) => samples = newSamples
                      case None => Dialog.showMessage(frame, s""""$str" is not a number""", "Error", Dialog.Message.Error)
                    }
                  }
                })
              )
            },
            new MenuItem(Action("Render") {
              thread match {
                case Some(thread) => thread.break()
                case None =>
                  thread = Some(new RenderThread(this))
                  thread.foreach(_.start())
              }
            }) {
              name = "Render"
            }
          )
        }

        contents = new BorderPanel {
          layout(renderPanel) = BorderPanel.Position.Center
          layout(statusBar) = BorderPanel.Position.South
        }

        pack()
        centerOnScreen()
      }

      frame.open()
      if (options.scene.isDefined && scene.isEmpty) {
        Dialog.showMessage(frame, s"Error loading scene ${options.scene()}", "Error", Dialog.Message.Error)
      }
      thread = Some(new RenderThread(frame.menuBar))
      thread.foreach(_.start())
  }

  def render(camera: Camera, world: Hittable,
             update: Int => Unit = _ => (),
             finish: Double => Unit = _ => (),
             shouldBreak: () => Boolean = () => false): Unit = {
    val start = System.currentTimeMillis()
    boundary:
      for (j <- 0 until height) {
        if (shouldBreak()) break()
        for (i <- 0 until width) {
          val pixel = Await.result(Future.reduceLeft(for (_ <- 0 until samples) yield Future {
            val u = (i + Random.nextDouble())/(width - 1)
            val v = (j + Random.nextDouble())/(height - 1)
            camera.ray_color(u, v, world)
          })(_ + _), Duration.Inf)/samples

          img.setRGB(i, height - j - 1, pixel.toRGB)
        }
        update(j)
      }
    val stop = System.currentTimeMillis()
    finish((stop - start)/1000.0)
  }
}
