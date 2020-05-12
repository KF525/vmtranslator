package translator.assemblycommand

trait Register extends AssemblyCommand
object Register {
  case class NameRegister(s: String) extends Register
  case class NumberRegister(i: Int) extends Register
  case class SpecialRegister(s: String) extends Register
  case class StaticRegister(fileName: String, i: Int) extends Register
}