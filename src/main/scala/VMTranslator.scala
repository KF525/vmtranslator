import cats.effect.{IO, Resource}
import parser.command.{InstructionComponent, VMCommand}
import parser.Parser
import parser.error.ParsingError

import scala.collection.immutable

//TODO: read in vm file, generate assembly file
object VMTranslator extends App {
    //    val vmCode = Resource.make(IO.apply(Source.fromResource("HelloWorld.vm")))(source => IO(source.close()))
    //    val assemblyCommands: List[String] = vmCode.use(generateAssemblyFile).unsafeRunSync()
}