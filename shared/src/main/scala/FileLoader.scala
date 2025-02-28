trait FileLoader {
  def loadImage(path: String): Image
  def loadFile(path: String): String
}
