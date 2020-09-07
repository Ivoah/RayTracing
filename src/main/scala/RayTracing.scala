import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object RayTracing extends App {

    case class Options(
        filename: Option[String] = Some("img.ppm"),
        width: Option[Int] = Some(400),
        height: Option[Int] = Some(225),
        samples: Option[Int] = Some(32),
        materials: Option[String] = None,
        world: Option[String] = None
    )

    @scala.annotation.tailrec
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
            case "--materials" :: materials :: tail => parseOptions(tail, options.map(_.copy(materials = Some(materials))))
            case "--world" :: world :: tail => parseOptions(tail, options.map(_.copy(world = Some(world))))
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
    val filename = options.filename.get
    val image_width = options.width.get
    val image_height = options.height.get
    val aspect_ratio = options.width.get.toDouble/options.height.get
    val samples = options.samples.get
    val max_depth = 50

    // Camera
    val origin = Vec3(13, 2, 3)
    val lookat = Vec3(0, 0, 0)
    val aperture = 0.1
    val focus_distance = 10
    val camera = Camera(
        origin, lookat, Vec3(0, 1, 0),
        20, aspect_ratio, aperture, focus_distance
    )

    val rand = new scala.util.Random

    val world = HittableList(
        Sphere(Vec3(0, -1000, 0), 1000, Diffuse(Vec3(0.5, 0.5, 0.5))) +:
        (for (a <- -11 until 11; b <- -11 until 11) yield {
            val choose_mat = rand.nextDouble()
            val material = if (choose_mat < 0.8) Diffuse(Vec3.random)
                else if (choose_mat < 0.95) Glossy(Vec3.random(0.5, 1), rand.between(0.0, 0.5))
                else Glass(Vec3(1, 1, 1), 1.5)
            val center = Vec3(a + 0.9*rand.nextDouble(), 0.2, b + 0.9*rand.nextDouble())
            if ((center - Vec3(4, 0.2, 0)).length > 0.9) Some(Sphere(center, 0.2, material))
            else None
        }).flatten :+
        Sphere(Vec3(0, 1, 0), 1, Glass(Vec3(1, 1, 1), 1.5)) :+
        Sphere(Vec3(-4, 1, 0), 1, Diffuse(Vec3(0.4, 0.2, 0.1))) :+
        Sphere(Vec3(4, 1, 0), 1, Glossy(Vec3(0.7, 0.6, 0.5), 0)): _*
    )

    val ppm = new java.io.PrintWriter(filename)
    ppm.write(s"P3\n$image_width $image_height\n255\n")

    for (j <- image_height - 1 to 0 by -1) {
        print(s"\rRendering line [${image_height - j}/$image_height]")
        for (i <- 0 until image_width) {
            val pixel = Await.result(Future.reduceLeft(for (_ <- 0 until samples) yield Future {
                val u = (i + rand.nextDouble())/(image_width - 1)
                val v = (j + rand.nextDouble())/(image_height - 1)
                camera.ray_color(u, v, world, max_depth)
            })(_ + _), Duration.Inf)/samples

            ppm.write(pixel.toPPM)
        }
    }
    println()

    ppm.close()
}
