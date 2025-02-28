import java.io.*
import java.nio.file.Files
import javax.imageio.ImageIO
import java.awt.image.DataBufferByte

given FileLoader = new FileLoader {
  // File doesn't respect the cwd so we manually append it if not an absolute path
  private def file(path: String): File = {
    val file = new File(path)
    if (file.isAbsolute) file
    else new File(System.getProperty("user.dir"), file.getName)
  }

  def loadImage(path: String): Image = {
    val img = ImageIO.read(file(path))
    Image(img.getWidth(), img.getHeight(), img.getRGB(0, 0, img.getWidth(), img.getHeight(), null, 0, img.getWidth()).map(Vec3.fromRGB).toIndexedSeq)
  }

  def loadFile(path: String): String = {
    Files.readString(file(path).toPath)
  }
}
