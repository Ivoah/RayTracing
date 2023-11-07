import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

@main
def main(args: String*): Unit = {

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
      println(s"Error parsing arguments \"$args\"")
      println(usage)
      System.exit(1)
  }

  if (options.help) println(usage)

  def formatDuration(t: Double) = {
    if (t >= 3600) f"${t/3600}%02.0fh${(t%3600)/60}%02.0fm${t%60}%05.2fs"
    else f"${(t%3600)/60}%02.0fm${t%60}%05.2fs"
  }

  val img = new BufferedImage(options.width, options.height, BufferedImage.TYPE_INT_RGB)

  options.filename match {
    case Some(filename) =>
      options.scene match {
        case Some(scene) =>
          Scene.fromFile(new File(scene)) match {
            case Some(Scene(camera, world)) =>
              camera.render(world, img, options.samples,
                line => print(s"\rRendered line [${line + 1}/${options.height}]"),
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
      val gui = RayTracingGUI(options)
      gui.open()
  }
}
