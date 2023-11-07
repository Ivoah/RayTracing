import java.awt.image.BufferedImage
import javax.imageio.ImageIO

@main
def main(args: String*): Unit = {

  val options = Options(args)
  
  options.output.toOption match {
    case Some(filename) =>
      options.scene.toOption match {
        case Some(scene) =>
          Scene.fromFile(scene, options.dump()) match {
            case Some(Scene(camera, world)) =>
              val img = new BufferedImage(options.width(), options.height(), BufferedImage.TYPE_INT_RGB)
              camera.render(world, img, options.samples(),
                line => print(s"\rRendered line [${line + 1}/${options.height()}]"),
                time => println(s"\nTime: ${Util.formatDuration(time)}")
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
