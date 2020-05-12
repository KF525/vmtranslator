package translator.assemblycommand

import translator.assemblycommand.Register.NameRegister

trait AssemblyCommand
object AssemblyCommand {
  case class RegisterAssignment(reg: RegisterExp, expression: Expression) extends AssemblyCommand
  case class JumpNotEqual(a: Value) extends AssemblyCommand
  case class JumpEqual(a: Value) extends AssemblyCommand
  case class JumpGreaterThan(a: Value) extends AssemblyCommand
  case class JumpGreaterThanOrEqual(a: Value) extends AssemblyCommand
  case class JumpLessThan(a: Value) extends AssemblyCommand
  case class JumpLessThanOrEqual(a: Value) extends AssemblyCommand
  case class UnconditionalJump(a: Value) extends AssemblyCommand
  case class LabelA(name: String) extends AssemblyCommand
  case class GoToA(register: NameRegister) extends AssemblyCommand
}