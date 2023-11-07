import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import javax.swing.filechooser.FileNameExtensionFilter
import scala.language.reflectiveCalls
import scala.reflect.Selectable.reflectiveSelectable
import scala.swing.*
import scala.swing.Swing.*

class RayTracingGUI(var options: Options) extends MainFrame() {
  var img = new BufferedImage(options.width, options.height, BufferedImage.TYPE_INT_RGB)

  def formatDuration(t: Double) = {
    if (t >= 3600) f"${t / 3600}%02.0fh${(t % 3600) / 60}%02.0fm${t % 60}%05.2fs"
    else f"${(t % 3600) / 60}%02.0fm${t % 60}%05.2fs"
  }

  val renderPanel = new Panel {
    preferredSize = (options.width, options.height)

    override def paintComponent(g: Graphics2D): Unit = {
      g.drawImage(img, 0, 0, null)
    }
  }

  val statusBar: GridPanel { val label: Label; val progressBar: ProgressBar; def setProgressBar(): Unit; def setLabel(): Unit } = new GridPanel(1, 1) {
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

  var scene = options.scene.flatMap(f => Scene.fromFile(new File(f)))

  var thread: Option[RenderThread] = None
  class RenderThread(val bar: MenuBar) extends Thread {
    private var _break = false
    override def run(): Unit = {
      val renderButton = bar.contents.find(_.name == "Render").get.asInstanceOf[MenuItem]
      scene match {
        case Some(Scene(camera, world)) =>
          bar.contents.foreach(_.enabled = false)
          renderButton.text = "Stop"
          renderButton.enabled = true
          statusBar.progressBar.max = options.height
          statusBar.setProgressBar()
          camera.render(world, img, options.samples,
            line => {
              statusBar.progressBar.value = line;
              frame.repaint()
            },
            time => {
              statusBar.label.text = s"Time ${formatDuration(time)}"
              statusBar.setLabel()
            },
            () => _break
          )
        case None =>
      }
      thread = None
      bar.contents.foreach(_.enabled = true)
      renderButton.text = "Render"
    }

    def break(): Unit = {_break = true}
  }

  private val frame = this

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
              scene = Scene.fromFile(chooser.selectedFile)
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

  if (options.scene.isDefined && scene.isEmpty) {
    Dialog.showMessage(frame, s"Error loading scene ${options.scene.get}", "Error", Dialog.Message.Error)
  }
  thread = Some(new RenderThread(frame.menuBar))
  thread.foreach(_.start())
}
