package muyan.diagramsketch

import org.scalatest.{FlatSpec, ShouldMatchers}

class DiagramSketchTest  extends FlatSpec with ShouldMatchers with DiagramSketch{

   "muyan" should "Test extract tokens results, and tree node deep length" in {
     val context = s"""package muyan.diagramsketch
                      class TestSample extends DiagramSketch {
                       class InnerClass {
                         def method(v: String = "") {}
                      }
                        var var1,var2:Int = 0
                      }"""
     tempNode.clear() //before running clear
     val in = sourceParser(context)
     in.topStats.otherStats.foreach(x => extractToken(x._2))

     val clz = tempNode.filter{ el => el.isInstanceOf[ClazzSketch] }
     clz.length shouldBe(2)
     clz.filter(_.deepLength.equals(1)).length shouldBe(1)

     val inherit = tempNode.filter{ el => el.isInstanceOf[InheritSketch]}
     inherit.length shouldBe(1)
     inherit.head.deepLength shouldBe(2)

     val method = tempNode.filter{ el => el.isInstanceOf[FunctionSketch]}
     method.head.deepLength shouldBe(5)

     val attr = tempNode.filter{ el => el.isInstanceOf[AttributeSketch]}
     attr.head.deepLength shouldBe(3)

     println(filter(classOf[FunctionSketch]))

       def filter(node : Class[_ <: SketchNode])  = {
         println(node.toString)
         tempNode.filter{ el => el.isInstanceOf[node.type]}

       }

   }

}
