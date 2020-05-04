package in.tap.we.ui

import cats.effect.IO
import org.scalajs.dom._
import outwatch._
import outwatch.dsl._

class TapinuiSpec extends JSDomSpec {

  "You" should "probably add some tests" in {

    val message = "Hello World!"
    OutWatch.renderInto[IO]("#app", h1(message)).unsafeRunSync()

    document.body.innerHTML.contains(message) shouldBe true
  }
}
