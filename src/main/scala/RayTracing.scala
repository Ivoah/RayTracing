import scala.util.Random
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing._
import scala.swing.Swing._
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import javax.swing.filechooser.FileNameExtensionFilter
import play.api.libs.json._

import java.nio.file.Files

object RayTracing extends App {

  case class Options(
    filename: Option[String] = None,
    width: Int = 400,
    height: Int = 225,
    samples: Int = 32,
    scene: Option[String] = None,
    dump: Boolean = false,
    help: Boolean = false
  )

  @annotation.tailrec
  def parseOptions(args: List[String], options: Options = Options()): Options = {
    args match {
      case "--filename" :: filename :: tail => parseOptions(tail, options.copy(filename = Some(filename)))
      case "-o" :: filename :: tail => parseOptions(tail, options.copy(filename = Some(filename)))
      case "--width" :: width :: tail => parseOptions(tail, options.copy(width = width.toInt))
      case "-w" :: width :: tail => parseOptions(tail, options.copy(width = width.toInt))
      case "--height" :: height :: tail => parseOptions(tail, options.copy(height = height.toInt))
      case "-h" :: height :: tail => parseOptions(tail, options.copy(height = height.toInt))
      case "--samples" :: samples :: tail => parseOptions(tail, options.copy(samples = samples.toInt))
      case "-s" :: samples :: tail => parseOptions(tail, options.copy(samples = samples.toInt))
      case "--scene" :: scene :: tail => parseOptions(tail, options.copy(scene = Some(scene)))
      case "--dump" :: tail => parseOptions(tail, options.copy(dump = true))
      case "--help" :: tail => parseOptions(tail, options.copy(help = true))
      case Nil => options
      case _ => throw new Exception("Error parsing arguments")
    }
  }

  val usage =
    s"""Options:
       |  --filename, -o
       |      Must specify --scene if giving output file
       |  --width, -w
       |  --height, -h
       |  --samples, -s
       |  --scene
       |  --dump
       |  --help""".stripMargin

  var options = Options()
  try {
    options = parseOptions(args.toList)
  } catch {
    case _: Throwable =>
      println("Error parsing arguments")
      println(usage)
      System.exit(1)
  }

  if (options.help) println(usage)

  def loadScene(scene: File) = {
    try {
      val json = Json.parse(Files.readString(scene.toPath))
      System.setProperty("user.dir", scene.getAbsoluteFile.getParent)

      import JsonFormats._
      val camera = (json \ "camera").as[Camera]
      implicit val materials: Map[String, Material] = (json \ "materials").as[Map[String, Material]]
      val world = HittableList((json \ "world").as[Seq[Hittable]]: _*)
      if (options.dump) pprint.pprintln(world)
      Some((camera, world))
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        None
    }
  }

  var img = new BufferedImage(options.width, options.height, BufferedImage.TYPE_INT_RGB)

  options.filename match {
    case Some(filename) =>
      options.scene match {
        case Some(scene) =>
          loadScene(new File(scene)) match {
            case Some((camera, world)) =>
              render(camera, world,
                Some(line => print(s"\rRendered line [${line + 1}/${options.height}]")),
                Some(time => println(s"\nRendered ${options.height} lines in $time seconds"))
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
        preferredSize = (options.width, options.height)

        override def paintComponent(g: Graphics2D): Unit = {
          g.drawImage(img, 0, 0, null)
        }
      }

      val statusBar = new GridPanel(1, 1) {
        val label: Label = new Label("") {
          xAlignment = Alignment.Left
        }
        val progressBar: ProgressBar = new ProgressBar {
          min = 0
          max = options.height
        }

        def setProgressBar(): Unit = contents(0) = progressBar
        def setLabel(): Unit = contents(0) = label

        contents += label
      }

      var scene = options.scene.flatMap(f => loadScene(new File(f)))

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
              statusBar.progressBar.max = options.height
              statusBar.setProgressBar()
              render(camera, world,
                Some(line => {statusBar.progressBar.value = line; frame.repaint()}),
                Some(time => {
                  statusBar.label.text = s"Rendered ${options.height} lines in $time seconds"
                  statusBar.setLabel()
                }),
                Some(() => !rendering)
              )
              thread = None
              bar.contents.foreach(_.enabled = true)
              renderButton.text = "Render"
            case None =>
          }
        }

        def break(): Unit = {
          rendering = false
        }
      }

      lazy val frame: Frame = new MainFrame {
        title = options.scene match {
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
                  Dialog.showInput(frame, "Image width", initial = options.width.toString).foreach { str =>
                    str.toIntOption match {
                      case Some(width) =>
                        options = options.copy(width = width)
                        renderPanel.preferredSize = (width, options.height)
                        renderPanel.revalidate()
                        frame.pack()
                        img = new BufferedImage(options.width, options.height, BufferedImage.TYPE_INT_RGB)
                      case None => Dialog.showMessage(frame, s"${'"'}$str${'"'} is not a number", "Error", Dialog.Message.Error)
                    }
                  }
                }),
                new MenuItem(Action("Height") {
                  Dialog.showInput(frame, "Image height", initial = options.height.toString).foreach { str =>
                    str.toIntOption match {
                      case Some(height) =>
                        options = options.copy(height = height)
                        renderPanel.preferredSize = (options.width, height)
                        renderPanel.revalidate()
                        frame.pack()
                        img = new BufferedImage(options.width, options.height, BufferedImage.TYPE_INT_RGB)
                      case None => Dialog.showMessage(frame, s"""""$str" is not a number""", "Error", Dialog.Message.Error)
                    }
                  }
                }),
                new MenuItem(Action("Samples") {
                  Dialog.showInput(frame, "Render samples", initial = options.samples.toString).foreach { str =>
                    str.toIntOption match {
                      case Some(samples) => options = options.copy(samples = samples)
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
        Dialog.showMessage(frame, s"Error loading scene ${options.scene.get}", "Error", Dialog.Message.Error)
      }
      thread = Some(new RenderThread(frame.menuBar))
      thread.map(_.start())
  }

  def render(camera: Camera, world: Hittable,
             update: Option[Int => Unit] = None,
             finish: Option[Double => Unit] = None,
             break: Option[() => Boolean] = None): Unit = {
    val start = System.currentTimeMillis()
    for (j <- 0 until options.height) {
      if (break.exists(_())) return
      for (i <- 0 until options.width) {
        val pixel = Await.result(Future.reduceLeft(for (_ <- 0 until options.samples) yield Future {
          val u = (i + Random.nextDouble())/(options.width - 1)
          val v = (j + Random.nextDouble())/(options.height - 1)
          camera.ray_color(u, v, world)
        })(_ + _), Duration.Inf)/options.samples

        img.setRGB(i, options.height - j - 1, pixel.toRGB)
      }
      update.foreach(_(j))
    }
    val stop = System.currentTimeMillis()
    finish.foreach(_((stop - start)/1000.0))
  }
}
