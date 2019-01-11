package muyan.graphviz

import org.scalatest.{FlatSpec, ShouldMatchers}

class GraphvizTest extends FlatSpec with ShouldMatchers{
  val gv = new Graphviz(None, "png")
    "tmpDir" should "test config tmpdir " in {
      gv.tmpDir.nonEmpty shouldBe(true)
      val dst = "./resources"
      gv.tmpDir shouldBe(dst)
    }

   "graphdir" should "test graph dir path" in {
     val expect = "C:\\Program Files (x86)\\Graphviz2.38\\bin\\dot.exe"
     expect shouldBe(gv.graphExecutable)
   }
}
