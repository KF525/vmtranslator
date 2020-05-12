package translator.assemblycommand

trait Expression extends AssemblyCommand
object Expression {
  case class AssignmentExpression(a: Value) extends Expression
  case class AddExpression(a: Value, b: Value) extends Expression
  case class SubtractExpression(a: Value, b: Value) extends Expression
  case class NegativeExpression(a: Value) extends Expression
  case class AndAssignmentExpression(a: Value, b: Value) extends Expression
  case class OrAssignmentExpression(a: Value, b: Value) extends Expression
  case class NotAssignmentExpression(a: Value) extends Expression
  case class LessThanExpression(a: Value, b: Value) extends Expression
  case class GreaterThanExpression(a: Value, b: Value) extends Expression
  case class EqualExpression(a: Value, b: Value) extends Expression
}