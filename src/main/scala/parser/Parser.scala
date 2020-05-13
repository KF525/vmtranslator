package parser

import parser.error.ParsingError
import parser.command.VMCommand._
import parser.command._
import scala.util.{Success, Try}

/** parses input string from program into a formal vm command */
object Parser {
  val VMCapturePattern = "^\\s*(\\w+\\S*\\w*)(?:\\s+(\\w+\\S*\\w*)\\s+(\\d+))?(?:\\s+(\\S+))?.*$".r

  def generateVMCommand(command: String): Either[ParsingError, VMCommand] = {
    VMCapturePattern.findFirstIn(command.trim()).toList.flatMap(_.split(" ")) match {
      case h::t => findCommand(h, t)
      case Nil => Left(ParsingError(s"Could not match command: $command."))
    }
  }

  private def findCommand(command: String, args: List[String]) : Either[ParsingError, VMCommand] = command match {
    case "push" => createMemoryCommand(args, Push)
    case "pop" => createMemoryCommand(args, Pop)
    case "goto" => createBranchingCommand(args, GoTo)
    case "label" => createBranchingCommand(args, Label)
    case "if-goto" => createBranchingCommand(args, IfGoTo)
    case "function" => createFunctionCommand(args, Function)
    case "call" => createFunctionCommand(args, FunctionCall)
    case "return" => Right(FunctionReturn)
    case vmCommand: String => VMCommand.fromString(vmCommand) match {
      case Some(c) => Right(c)
      case None => Left(ParsingError(s"Could not match: $command."))
    }
    case _ => Left(ParsingError(s"Could not match: $command."))
  }

    private def createBranchingCommand(args: List[String],
                                       createFunction: String => VMCommand): Either[ParsingError, VMCommand] =
      args match {
        case name::_ => Right(createFunction(name))
        case _ => Left(ParsingError(s"Could not parse: $createFunction"))
      }

    private def createFunctionCommand(args: List[String],
                                      createFunction: (String, Int) => VMCommand): Either[ParsingError, VMCommand] =
      args match {
        case name::location::_ => Try(location.toInt) match {
          case Success(i) => Right(createFunction(name, i))
          case _ => Left(ParsingError(s"Command location: $location invalid."))
        }
        case _ => Left(ParsingError(s"Invalid arguments: $args for $createFunction."))
      }

    private def createMemoryCommand(args: List[String],
                                    createFunction: (Segment, Int) => VMCommand): Either[ParsingError, VMCommand] =
      args match {
        case segment::location:: _ =>
          val segmentOption = Segment.fromString(segment)
          val locationTry = Try(location.toInt)
          if (segmentOption.isDefined && locationTry.isSuccess) {
            Right(createFunction(segmentOption.get, locationTry.get))
          } else {
            Left(ParsingError(s"Invalid arguments: $args for $createFunction."))
          }
        case _ => Left(ParsingError(s"Invalid arguments: $args for $createFunction."))
      }
}