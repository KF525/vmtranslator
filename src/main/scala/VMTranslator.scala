import cats.effect.{IO, Resource}

object VMTranslator extends App {

  def generateAssembly(): Unit = {
    val vmCode = IO { scala.io.Source.fromResource("HelloWorld.vm") }

    Resource.fromAutoCloseable(vmCode)
      .use(source => IO(println(source.getLines().toList.map(t => t + " More"))))
      .unsafeRunSync()
  }
}
