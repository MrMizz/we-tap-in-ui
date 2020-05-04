package in.tap.we.ui

import org.scalajs.dom._
import org.scalatest._

abstract class JSDomSpec extends FlatSpec with Matchers with BeforeAndAfterEach {

  override def beforeEach(): Unit = {

    document.body.innerHTML = ""

    // prepare body with <div id="app"></div>
    val root = document.createElement("div")
    root.id = "app"
    document.body.appendChild(root)
    ()
  }
}
