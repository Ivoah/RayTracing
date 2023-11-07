case class Options(
                    filename: Option[String] = None,
                    width: Int = 400,
                    height: Int = 225,
                    samples: Int = 32,
                    scene: Option[String] = None,
                    dump: Boolean = false,
                    help: Boolean = false
                  )
