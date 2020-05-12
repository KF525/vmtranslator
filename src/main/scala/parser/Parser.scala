package parser

import parser.error.ParsingError
import parser.command.VMCommand._
import parser.command._
import scala.util.{Success, Try}
import scala.util.matching.Regex.Match

/** parses input string from program into a formal vm command */
object Parser {
  val arithmeticInstructions = List("add", "sub", "neg")
  val logicalInstructions = List("and", "or", "not", "gt", "eq", "lt")
  val VMCapturePattern = "^\\s*(\\w+\\S*\\w*)(?:\\s+(\\w+\\S*\\w*)\\s+(\\d+))?(?:\\s+(\\S+))?.*$".r

//  def generateVMInstruction(command: String): Either[ParsingError, VMCommand] = {
//    val maybeRegexMatch: List[String] = VMCapturePattern.findAllIn(command).toList
//    maybeRegexMatch match {
//      case h::t => findVMCommand(h, t)
//      case Nil => Left(ParsingError(s"Could not match command: $command."))
//    }
//  }

//  private def findVMCommand(command: String, args: List[String] : Either[ParsingError, VMCommand] = ???

  def generateVMInstruction(command: String): Either[ParsingError, VMCommand] = {
    val maybeRegexMatch = VMCapturePattern.findFirstMatchIn(command)
    maybeRegexMatch match {
      case Some(regexMatch) => findVMCommand(regexMatch)
      case None => Left(ParsingError(s"Could not match command: $command."))
    }
  }


  private def findVMCommand(result: Match) =
    result.group(1).toLowerCase() match {
      case "push" => createMemoryCommand(result, Push)
      case "pop" => createMemoryCommand(result, Pop)
      case "goto" => createBranchingCommand(result)
      case "label" => createBranchingCommand(result)
      case "if-goto" => createBranchingCommand(result)
      case "function" => createFunctionCommand(result)
      case "call" => createFunctionCommand(result)
      case "return" => Right(FunctionReturn)
      case s: String => VMCommand.fromString(s) match {
        case Some(v) => Right(v)
        case None => Left(ParsingError(s"Could not match: $result."))
      }
      case _ => Left(ParsingError(s"Could not match: $result."))
    }

  private def createBranchingCommand(result: Match) = {
    val branchType = result.group(1)
    val name = result.group(4)

    branchType match {
      case "goto" => Right(GoTo(name))
      case "label" => Right(Label(name))
      case "if-goto" => Right(IfGoTo(name))
      case _ => Left(ParsingError(s"Could not parse: $result"))
    }
  }

  private def createFunctionCommand(result: Match) = result.group(1) match {
    case "function" =>
      val functionName = result.group(2)
      val locationTry = Try(result.group(3).toInt)
      locationTry match {
        case Success(i) => Right(Function(functionName, i))
        case _ => Left(ParsingError(s"Invalid number of arguments ${result.group(3)} for $result."))
      }
    case "call" =>
      val functionName = result.group(2)
      val locationTry = Try(result.group(3).toInt)
      locationTry match {
        case Success(i) => Right(FunctionCall(functionName, i))
        case _ => Left(ParsingError(s"Invalid number of arguments ${result.group(3)} for $result."))
      }
  }

  private def createMemoryCommand(result: Match, createFunction: (Segment, Int) => VMCommand) =
    Segment.fromString(result.group(2)) match {
      case Some(segment) =>
        val locationTry = Try(result.group(3).toInt)
        locationTry match {
          case Success(i) => Right(createFunction(segment, i))
          case _ => Left(ParsingError(s"Invalid location ${result.group(3)} for $result."))
        }
      case None => Left(ParsingError(s"Could not find segment: ${result.group(2)} for $result."))
    }
}