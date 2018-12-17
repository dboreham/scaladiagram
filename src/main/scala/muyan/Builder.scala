package muyan

import muyan.diagramsketch.{DiagramLite, DiagramSketch}
import muyan.graphviz.Graphviz
import muyan.template.DigraphBase
import muyan.template.EdgeGraph._

import scala.collection.mutable.ListBuffer


class Builder(path: String) extends Graphviz with DiagramSketch {
   require(new File(path).isFile)

  implicit def toSting(t: List[String]) :String = t.mkString("\n")

  def fileSketch = catalystSketch(path)

  //todo parse function innerclass inherit class etc.
  //todo SketchNode add parsing function, every class override it.
  def dotBuilder(buff: ListBuffer[DiagramLite]) =  {
    buff.foreach{
      case DiagramLite(clz, ext, inner, method) =>
         val extCtx: List[String] = if(ext.isDefined) ext.get.descSketch else Nil

        addItem(clz.descSketch, method.map( e => s"${e.descSketch.mkString("")}"),"")
        (extCtx.filterNot(clz.descSketch.contains(_))).foreach {
          name => addItem(name, Nil,"")
        }

        extCtx.foreach {
            o => addRelation(clz.descSketch, o, inherit)
          }

        inner.foreach {el =>
          addRelation(clz.descSketch, el.descSketch, composition)}
    }
  }

  def build: Unit = {
   val sketch = catalystSketch(path)
    val file = new File(path).getName
    dotBuilder(sketch)
    println(graphContent)
    draw(file)
  }


}
