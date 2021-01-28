import java.io._
import java.net._
import scala.util.{Random, Try}

object RenderServer extends App {
  case class RenderRequest(camera: Option[Camera], world: Option[Hittable], options: Option[RayTracing.Options], line: Int)

  class RenderThread(val socket: Socket, val id: Int) extends Thread {
    var lastCamera: Option[Camera] = None
    var lastWorld: Option[Hittable] = None
    var lastOptions: Option[RayTracing.Options] = None

    override def run(): Unit = {
      val in = new ObjectInputStream(socket.getInputStream)
      val out = new ObjectOutputStream(socket.getOutputStream)

      while (socket.isConnected) {
        val line = Try {
          val request = in.readObject().asInstanceOf[RenderRequest]

          val camera = request.camera.orElse(lastCamera).get
          val world = request.world.orElse(lastWorld).get
          val options = request.options.orElse(lastOptions).get

          lastCamera = Some(camera)
          lastWorld = Some(world)
          lastOptions = Some(options)

          val line: Seq[Int] = for (i <- 0 until options.width) yield {
            val pixel = (for (_ <- 0 until options.samples) yield {
              val u = (i + Random.nextDouble()) / (options.width - 1)
              val v = (request.line + Random.nextDouble()) / (options.height - 1)
              camera.ray_color(u, v, world)
            }).reduce(_ + _) / options.samples

            pixel.toRGB
          }
          println(s"Rendered line [${request.line + 1}/${options.height}] on thread $id")
          line
        }

        Try {
          out.writeObject(line)
          out.flush()
        }
      }
      socket.close()
    }
  }

  val server = new ServerSocket(5122)
  var nextId = 0
  while (!server.isClosed) {
    val socket = server.accept()
    val thread = new RenderThread(socket, nextId)
    thread.start()
    nextId += 1
  }
  server.close()
}
