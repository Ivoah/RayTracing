import org.rogach.scallop.*

import java.io.File

class Options(args: Seq[String]) extends ScallopConf(args) {
  val output: ScallopOption[File] = opt[File]()
  val width: ScallopOption[Int] = opt[Int](default = Some(400))
  val height: ScallopOption[Int] = opt[Int](default = Some(225))
  val samples: ScallopOption[Int] = opt[Int](default = Some(32), noshort = true)
  val scene: ScallopOption[File] = opt[File]()
  val dump: ScallopOption[Boolean] = opt[Boolean]()

  dependsOnAll(output, List(scene))
  verify()
}
