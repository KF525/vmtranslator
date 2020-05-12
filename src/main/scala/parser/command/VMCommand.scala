package parser.command

sealed abstract class VMCommand(val name: String) extends InstructionComponent
object VMCommand {
  case object InitSP extends VMCommand("init")
  case class Pop(segment: Segment, location: Int) extends VMCommand("pop")
  case class Push(segment: Segment, location: Int) extends VMCommand("push")
  case object Add extends VMCommand("add")
  case object Subtract extends VMCommand("sub")
  case object Negative extends VMCommand("neg")
  case object And extends VMCommand("and")
  case object Or extends VMCommand("or")
  case object Not extends VMCommand("not")
  case object Equal extends VMCommand("eq")
  case object LessThan extends VMCommand("lt")
  case object GreaterThan extends VMCommand("gt")
  case object FunctionReturn extends VMCommand("return")
  case class FunctionCall(functionName: String, nArgs: Int) extends VMCommand("call")
  case class Function(functionName: String, localVars: Int) extends VMCommand("function")
  case class GoTo(variable: String) extends VMCommand("goto")
  case class IfGoTo(variable: String) extends VMCommand("if-goto")
  case class Label(variable: String) extends VMCommand("label")

  def fromString(s: String): Option[VMCommand] =
    Seq(Add, Subtract, Negative, Equal, GreaterThan, LessThan, And, Or, Not, InitSP).find(_.name == s)
}
