package in.tap.we.ui

import cats.effect.IO
import outwatch.OutWatch
import outwatch.dsl.h1

object Tapinui {

  def main(args: Array[String]): Unit = {

    OutWatch.renderInto[IO]("#app", h1("Hello World")).unsafeRunSync()
  }
}
