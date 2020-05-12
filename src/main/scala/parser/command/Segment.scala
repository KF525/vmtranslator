package parser.command

sealed abstract class Segment( val name: String, val label: Option[String]) extends InstructionComponent
object Segment {
  case object Constant extends Segment("constant",None)
  case object Local extends Segment("local",Some("LCL"))
  case object Argument extends Segment("argument",Some("ARG"))
  case object This extends Segment("this",Some("THIS"))
  case object That extends Segment("that",Some("THAT"))
  case object Pointer extends Segment("pointer",None)
  case object Static extends Segment("static", None)
  case object Temp extends Segment("temp", None)

  def fromString(s: String): Option[Segment] =
    Seq(Static, Local, This, That, Constant, Pointer, Temp, Argument).find(_.toString.equalsIgnoreCase(s))
}

