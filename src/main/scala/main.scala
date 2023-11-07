import java.awt.image.BufferedImage
import javax.imageio.ImageIO

@main
def main(args: String*): Unit = {

  val options = Options(args)

  def formatDuration(t: Double) = {
    if (t >= 3600) f"${t/3600}%02.0fh${(t%3600)/60}%02.0fm${t%60}%05.2fs"
    else f"${(t%3600)/60}%02.0fm${t%60}%05.2fs"
  }

  val img = new BufferedImage(options.width(), options.height(), BufferedImage.TYPE_INT_RGB)

  options.filename.toOption match {
    case Some(filename) =>
      options.scene.toOption match {
        case Some(scene) =>
          Scene.fromFile(scene, options.dump()) match {
            case Some(Scene(camera, world)) =>
              camera.render(world, img, options.samples(),
                line => print(s"\rRendered line [${line + 1}/${options.height}]"),
                time => println(s"\nTime: ${formatDuration(time)}")
              )
              ImageIO.write(img, "png", filename)
            case None =>
              println(s"Error loading scene $scene")
              System.exit(1)
          }
        case None =>
          println("No scene file specified")
          System.exit(1)
      }
    case None =>
      val gui = RayTracingGUI(options)
      gui.open()
  }
}
