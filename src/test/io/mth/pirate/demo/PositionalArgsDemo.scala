package io.mth.pirate.demo


object PositionalArgsDemo {
  case class DemoArgs(help: Boolean, version: Boolean, verbose: Boolean, expression: Option[String], console: Boolean, command: Option[String], config: List[String])

  import io.mth.pirate._

  val cmd =
    command[DemoArgs]("demo") <|>
      full('h', "--help", "display usage.")(_.copy(help = true)) <|>
      full('V', "--version", "display version.")(_.copy(version = true)) <|>
      full('v', "--verbose", "verbose output.")(_.copy(verbose = true)) >|
      positional("COMMAND")((d, s) => d.copy(command = Some(s))) >|
      positional0plus("CONFIG")((d, ss) => d.copy(config = ss))

  val program = cmd ~
      """
      |This is a demo program. COMMAND represents some program command
      |and CONFIG represents a config file.
      """.stripMargin

  def main(args: Array[String]) {
    println(program.usage)

    val result = program.parse(List("--version", "-h", "go", "config1.file", "config2.file"), DemoArgs(false, false, false, None, false, None, List()))

    println("Parsed: \n        " + result)
  }
}