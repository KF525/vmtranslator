package translator

import parser.error.ParsingError
import parser.command.Segment._
import parser.command.VMCommand
import parser.command.VMCommand._
import translator.assemblycommand.AssemblyCommand._
import translator.assemblycommand.Expression._
import translator.assemblycommand.Register._
import translator.assemblycommand._
import translator.error.TranslationError

/** translates vm instruction into assembly instructions */
class Translator {
  val DRegister = RegisterExp(SpecialRegister("D"))
  val MRegister = RegisterExp(SpecialRegister("M"))
  val ARegister = RegisterExp(SpecialRegister("A"))
  val ARegisterValue = Value(Left(RegisterExp(SpecialRegister("A"))))
  val DRegisterValue = Value(Left(RegisterExp(SpecialRegister("D"))))
  val MRegisterValue = Value(Left(RegisterExp(SpecialRegister("M"))))
  val Const0RegisterValue = Value(Right(ConstantExp(0)))
  val Const1RegisterValue = Value(Right(ConstantExp(1)))
  val ConstNeg1RegisterValue = Value(Right(ConstantExp(-1)))
  val popVariable = "v"
  val stackPointer = "SP"
  val argument = "ARG"
  val local = "LCL"

  def generateAssembly(instruction: Either[ParsingError, VMCommand], fileName: String, current: Int = 0): Either[TranslationError, List[AssemblyCommand]] =
    instruction match {
      case Right(i) => translateToAssembly(i, fileName, current)
      case Left(parsingError) => Left(TranslationError(s"Could not translate parsing error: $parsingError"))
    }

  private def translateToAssembly(instruction: VMCommand, fileName: String, current: Int) = instruction match {
    case InitSP => Right(generateInitSP)
    case push: Push => Right(generatePushAssembly(push, fileName))
    case pop: Pop => Right(generatePopAssembly(pop, fileName))
    case Add => Right(generateArithmeticAssembly(AddExpression(MRegisterValue, DRegisterValue)))
    case Subtract => Right(generateArithmeticAssembly(SubtractExpression(MRegisterValue, DRegisterValue)))
    case Negative => Right(generateNegativeAssembly)
    case Equal => Right(generateJumpAssembly("EQ", JumpEqual(DRegisterValue), current))
    case LessThan => Right(generateJumpAssembly("LT", JumpLessThan(DRegisterValue), current))
    case GreaterThan => Right(generateJumpAssembly("GT", JumpGreaterThan(DRegisterValue), current))
    case Not => Right(generateNotAssembly)
    case And => Right(generateLogicAssembly(AndAssignmentExpression(DRegisterValue, MRegisterValue)))
    case Or => Right(generateLogicAssembly(OrAssignmentExpression(DRegisterValue, MRegisterValue)))
    case goto: GoTo => Right(generateGoTo(goto.variable))
    case ifGoTo: IfGoTo => Right(generateIfGoTo(ifGoTo.variable))
    case label: Label => Right(generateLabel(label.variable))
    case FunctionReturn => Right(generateFunctionReturn(current))
    case function: Function => Right(generateFunction(function.functionName, function.localVars, fileName))
    case functionCall: FunctionCall => Right(generateFunctionCall(functionCall.functionName, functionCall.nArgs, current))
    case _ => Left(TranslationError(s"Could not translate vmCommand: $instruction."))
  }

  private def generateInitSP = List(
    ConstantExp(256),
    RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
    RegisterExp(NameRegister(stackPointer)),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue))
  )

  private def generateFunction(functionName: String, localVars: Int, fileName: String, assemblyCommands: List[AssemblyCommand] = List()): List[AssemblyCommand] =
    localVars match {
      case 0 => List(LabelA(functionName)) ++ assemblyCommands
      case _ => generateFunction(functionName, localVars - 1, fileName, assemblyCommands ++ generatePushAssembly(Push(Constant, 0), fileName))
    }

  private def saveSegmentPlacetoStack(segment: String, value: Value) = List(
    RegisterExp(NameRegister(segment)),
    RegisterAssignment(DRegister, AssignmentExpression(value)),
    RegisterExp(NameRegister(stackPointer)),
    RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
    RegisterExp(NameRegister(stackPointer)),
    RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
  )

  private def generateFunctionCall(functionName: String, args: Int, current: Int) = {
    List(
      RegisterExp(NameRegister(s"RET$current")),
      RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
    ) ++
      saveSegmentPlacetoStack(local, MRegisterValue) ++
      saveSegmentPlacetoStack(argument, MRegisterValue) ++
      saveSegmentPlacetoStack("THIS", MRegisterValue) ++
      saveSegmentPlacetoStack("THAT", MRegisterValue) ++
      List(
        ConstantExp(5),
        RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
        RegisterExp(NameRegister(stackPointer)),
        RegisterAssignment(DRegister, SubtractExpression(MRegisterValue, DRegisterValue)),
        ConstantExp(args),
        RegisterAssignment(DRegister, SubtractExpression(DRegisterValue, ARegisterValue)),
        RegisterExp(NameRegister(argument)),
        RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
        RegisterExp(NameRegister(stackPointer)),
        RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
        RegisterExp(NameRegister(local)),
        RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue))
      ) ++
      generateGoTo(functionName) ++
      generateLabel(s"RET$current")

  }

  private def generateFunctionReturn(current: Int) = List(
    RegisterExp(NameRegister(local)),
    RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
    RegisterExp(NameRegister(s"ENDFRAME$current")),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
    ConstantExp(5),
    RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
    RegisterExp(NameRegister(s"ENDFRAME$current")),
    RegisterAssignment(ARegister, SubtractExpression(MRegisterValue, DRegisterValue)),
    RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
    RegisterExp(NameRegister(s"RET$current")),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
    RegisterExp(NameRegister(stackPointer)),
    RegisterAssignment(ARegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
    RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
    RegisterExp(NameRegister(argument)),
    RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
    RegisterExp(NameRegister(argument)),
    RegisterAssignment(DRegister, AddExpression(MRegisterValue, Const1RegisterValue)),
    RegisterExp(NameRegister(stackPointer)),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),

    ConstantExp(1),
    RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
    RegisterExp(NameRegister(s"ENDFRAME$current")),
    RegisterAssignment(ARegister, SubtractExpression(MRegisterValue, DRegisterValue)),
    RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
    RegisterExp(NameRegister("THAT")),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),

    ConstantExp(2),
    RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
    RegisterExp(NameRegister(s"ENDFRAME$current")),
    RegisterAssignment(ARegister, SubtractExpression(MRegisterValue, DRegisterValue)),
    RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
    RegisterExp(NameRegister("THIS")),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),

    ConstantExp(3),
    RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
    RegisterExp(NameRegister(s"ENDFRAME$current")),
    RegisterAssignment(ARegister, SubtractExpression(MRegisterValue, DRegisterValue)),
    RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
    RegisterExp(NameRegister(argument)),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),

    ConstantExp(4),
    RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
    RegisterExp(NameRegister(s"ENDFRAME$current")),
    RegisterAssignment(ARegister, SubtractExpression(MRegisterValue, DRegisterValue)),
    RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
    RegisterExp(NameRegister(local)),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),

    GoToA(NameRegister(s"RET$current")),
    RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
    UnconditionalJump(Const0RegisterValue)
  )

  private def generateGoTo(str: String): List[AssemblyCommand] = List(
    GoToA(NameRegister(str)),
    UnconditionalJump(Const0RegisterValue)
  )

  private def generateLabel(str: String) = List(
    LabelA(str)
  )

  private def generateIfGoTo(str: String) = List(
    RegisterExp(NameRegister(stackPointer)),
    RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
    RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
    RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
    RegisterExp(NameRegister(str)),
    JumpNotEqual(DRegisterValue)
  )

  private def generatePushAssembly(push: Push, fileName: String): List[AssemblyCommand] = {
    if (push.segment.equals(Pointer)) {
      val segment: String = if (push.location.equals(0)) "THIS" else "THAT"
      List(
        RegisterExp(NameRegister(segment)),
        RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue))
      ) ++ addToStack
    } else if (push.segment.equals(Temp)) {
      List(
        RegisterExp(NumberRegister(5)),
        RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
        RegisterExp(NumberRegister(push.location)),
        RegisterAssignment(ARegister, AddExpression(DRegisterValue, ARegisterValue)),
        RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue))
      ) ++ addToStack
    } else if (push.segment.equals(Constant)) { //confirm this is correct
      List(
        RegisterExp(NumberRegister(push.location)),
        RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue))
      ) ++ addToStack
    } else if (push.segment.equals(Static)) {
      List(
        RegisterExp(StaticRegister(fileName, push.location)),
        RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
        RegisterExp(NameRegister(stackPointer)),
        RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
        RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
        RegisterExp(NameRegister(stackPointer)),
        RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
      )
    } else {
      List(
        RegisterExp(NameRegister(push.segment.label.get)),
        RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
        RegisterExp(NumberRegister(push.location)),
        RegisterAssignment(ARegister, AddExpression(DRegisterValue, ARegisterValue)),
        RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue))
      ) ++ addToStack
    }
  }

  private def addToStack = List(
    RegisterExp(NameRegister(stackPointer)),
    RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
    RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
    RegisterExp(NameRegister(stackPointer)),
    RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
  )

  private def generatePopAssembly(pop: Pop, fileName: String): List[AssemblyCommand] = {
    if (pop.segment.equals(Pointer)) {
      val segment: String = if (pop.location.equals(0)) "THIS" else "THAT"
      List(
        RegisterExp(NameRegister(stackPointer)),
        RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
        RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
        RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
        RegisterExp(NameRegister(segment)),
        RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue))
      )
    } else if (pop.segment.equals(Static)){
      List(
        RegisterExp(NameRegister(stackPointer)),
        RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
        RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
        RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
        RegisterExp(StaticRegister(fileName, pop.location)),
        RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue))
      )
    } else {
      val segmentLocation = if (pop.segment.equals(Temp)) {
        List(ConstantExp(5),
          RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)))
      } else {
        List(RegisterExp(NameRegister(pop.segment.label.get)),
          RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)))
      }
      segmentLocation ++ List(
        ConstantExp(pop.location),
        RegisterAssignment(ARegister, AddExpression(ARegisterValue, DRegisterValue)),
        RegisterAssignment(DRegister, AssignmentExpression(ARegisterValue)),
        RegisterExp(NameRegister(popVariable)),
        RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
        RegisterExp(NameRegister(stackPointer)),
        RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
        RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
        RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
        RegisterExp(NameRegister(popVariable)),
        RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
        RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue))
      )
    }
  }

  private def generateArithmeticAssembly(expression: Expression): List[AssemblyCommand] =
    List(
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Value(Right(ConstantExp(1))))),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Value(Right(ConstantExp(1))))),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, expression),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
    )

  private def generateNegativeAssembly =
    List(
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Value(Right(ConstantExp(1))))),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, NegativeExpression(MRegisterValue)),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
    )

  private def generateNotAssembly =
    List(
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(ARegister, SubtractExpression(DRegisterValue, Const1RegisterValue)),
      RegisterAssignment(MRegister, NotAssignmentExpression(MRegisterValue))
    )

  private def generateLogicAssembly(expression: Expression) = //need a label here
    List(
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Value(Right(ConstantExp(1))))),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Value(Right(ConstantExp(1))))),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, expression),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
    )

  private def generateJumpAssembly(predicate: String, jumpCondition: AssemblyCommand, current: Int) = {
    List(
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(DRegister, SubtractExpression(MRegisterValue, DRegisterValue)),
      GoToA(NameRegister(s"$predicate$current")),
      jumpCondition,
      RegisterAssignment(DRegister, AssignmentExpression(Const0RegisterValue)),
      GoToA(NameRegister(s"END$current")),
      JumpEqual(Const0RegisterValue),
      LabelA(s"$predicate$current"),
      RegisterAssignment(DRegister, AssignmentExpression(ConstNeg1RegisterValue)),
      LabelA(s"END$current"),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(ARegister, AssignmentExpression(MRegisterValue)),
      RegisterAssignment(MRegister, AssignmentExpression(DRegisterValue)),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, AddExpression(MRegisterValue, Const1RegisterValue))
    )
  }

  private def popFrom: List[AssemblyCommand] =
    List(
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(ARegister, SubtractExpression(MRegisterValue, Const1RegisterValue)),
      RegisterAssignment(DRegister, AssignmentExpression(MRegisterValue)),
      RegisterExp(NameRegister(stackPointer)),
      RegisterAssignment(MRegister, SubtractExpression(MRegisterValue, Const1RegisterValue))
    )
}
