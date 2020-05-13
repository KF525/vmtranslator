package parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import parser.error.ParsingError
import parser.command.Segment.Static
import parser.command.VMCommand._

class ParserTest extends AnyFlatSpec with Matchers {

  "generateVMInstruction" should "return the correct VM Command" in {
    Parser.generateVMCommand("add") shouldBe Right(Add)
  }

  it should "ignore white space" in {
    Parser.generateVMCommand("   add") shouldBe Right(Add)
    Parser.generateVMCommand("add   ") shouldBe Right(Add)
  }

  it should "handle non-matches" in {
    Parser.generateVMCommand("bad") shouldBe Left(ParsingError("Could not match: bad."))
    Parser.generateVMCommand("pop bad 4") shouldBe Left(ParsingError("Invalid arguments: List(bad, 4) for Pop."))
  }

  it should "handle valid inputs correctly" in {
    Parser.generateVMCommand("pop static 4") shouldBe Right(Pop(Static, 4))
    Parser.generateVMCommand("push static 4") shouldBe Right(Push(Static, 4))
    Parser.generateVMCommand("goto START_LOOP") shouldBe Right(GoTo("START_LOOP"))
    Parser.generateVMCommand("if-goto START_LOOP") shouldBe Right(IfGoTo("START_LOOP"))
    Parser.generateVMCommand("label START_LOOP") shouldBe Right(Label("START_LOOP"))
    Parser.generateVMCommand("add") shouldBe Right(Add)
    Parser.generateVMCommand("sub") shouldBe Right(Subtract)
    Parser.generateVMCommand("neg") shouldBe Right(Negative)
    Parser.generateVMCommand("not") shouldBe Right(Not)
    Parser.generateVMCommand("and") shouldBe Right(And)
    Parser.generateVMCommand("or") shouldBe Right(Or)
    Parser.generateVMCommand("eq") shouldBe Right(Equal)
    Parser.generateVMCommand("gt") shouldBe Right(GreaterThan)
    Parser.generateVMCommand("lt") shouldBe Right(LessThan)
  }
}
