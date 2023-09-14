// https://github.com/OpenXiangShan/fudian
package darecreek.exu.vfu.fp.fudian

object ArgParser {

  val help =
    """
      |Usage:
      |--fu <name>
      |--ftype <32|64>
      |
      |example: --fu FADD -ftype 64 -td build
      |""".stripMargin

  /**
    * parse command args
    * @param args command line args
    * @return (()=>fu, firrtlOpts)
    */
  def parse(args: Array[String]): (String, Int, Int, Array[String]) = {
    var firrtpOpts = List[String]()
    var expWidth:  Option[Int] = None
    var precision: Option[Int] = None
    var module:    Option[String] = None
    def nextArg(list: List[String]): Unit = list match {
      case "--fu" :: name :: tail =>
        module = Some(name)
        nextArg(tail)
      case "--ftype" :: "32" :: tail =>
        expWidth = Some(8)
        precision = Some(24)
        nextArg(tail)
      case "--ftype" :: "64" :: tail =>
        expWidth = Some(11)
        precision = Some(53)
        nextArg(tail)
      case "--ftype" :: "32_64" :: tail =>
        expWidth = Some(-1)
        precision = Some(-1)
        nextArg(tail)
      case "--ftype" :: "64_32" :: tail =>
        expWidth = Some(-2)
        precision = Some(-2)
        nextArg(tail)
      case unknown :: tail =>
        firrtpOpts :+= unknown
        nextArg(tail)
      case Nil =>
    }
    nextArg(args.toList)
    require(module.nonEmpty && expWidth.nonEmpty && precision.nonEmpty, help)
    (module.get, expWidth.get, precision.get, firrtpOpts.toArray)
  }

}
