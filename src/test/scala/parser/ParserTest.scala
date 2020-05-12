package parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import parser.error.ParsingError
import parser.command.Segment.Static
import parser.command.VMCommand._
import parser.command.Location

class ParserTest extends AnyFlatSpec with Matchers {

  "generateVMInstruction" should "return the correct VM Command" in {
    Parser.generateVMInstruction("add") shouldBe Right(Add)
  }

  it should "ignore white space" in {
    Parser.generateVMInstruction("   add") shouldBe Right(Add)
    Parser.generateVMInstruction("add   ") shouldBe Right(Add)
  }

  it should "handle non-matches" in {
    Parser.generateVMInstruction("bad") shouldBe Left(ParsingError("Could not match: bad."))
    Parser.generateVMInstruction("pop bad 4") shouldBe Left(ParsingError("Could not find segment: bad for pop bad 4."))
  }

  it should "handle valid inputs correctly" in {
    Parser.generateVMInstruction("pop static 4") shouldBe Right(Pop(Static, 4))
    Parser.generateVMInstruction("push static 4") shouldBe Right(Push(Static, 4))
    Parser.generateVMInstruction("goto START_LOOP") shouldBe Right(GoTo("START_LOOP"))
    Parser.generateVMInstruction("if-goto START_LOOP") shouldBe Right(IfGoTo("START_LOOP"))
    Parser.generateVMInstruction("label START_LOOP") shouldBe Right(Label("START_LOOP"))
    Parser.generateVMInstruction("add") shouldBe Right(Add)
    Parser.generateVMInstruction("sub") shouldBe Right(Subtract)
    Parser.generateVMInstruction("neg") shouldBe Right(Negative)
    Parser.generateVMInstruction("not") shouldBe Right(Not)
    Parser.generateVMInstruction("and") shouldBe Right(And)
    Parser.generateVMInstruction("or") shouldBe Right(Or)
    Parser.generateVMInstruction("eq") shouldBe Right(Equal)
    Parser.generateVMInstruction("gt") shouldBe Right(GreaterThan)
    Parser.generateVMInstruction("lt") shouldBe Right(LessThan)
  }
}
