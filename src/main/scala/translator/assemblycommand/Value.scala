package translator.assemblycommand

case class Value(registerOrConstant: Either[RegisterExp, ConstantExp])
