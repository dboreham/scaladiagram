package muyan

import java.io.File

import muyan.diagramsketch.{DiagramLite, DiagramSketch}
import muyan.graphviz.Graphviz
import muyan.template.EdgeGraph._

import scala.collection.mutable.ListBuffer


class Builder(path: String, dst: Option[String], format: String) extends Graphviz(dst, format) with DiagramSketch {
  require(new File(path).isFile)

  implicit def toSting(t: List[String]) :String = t.mkString//("\n")

  def fileSketch = catalystSketch(path)


 private [this]def dotBuilder(buff: ListBuffer[DiagramLite]) =  {
    buff.foreach{
      case DiagramLite(clz, ext, inner, method, attr, _) =>
        addItem(clz.descSketch, method.flatMap( e => e.descSketch), attr.flatMap( e => e.descSketch))
       
       val extCtx: List[String] = if(ext.isDefined) ext.get.descSketch else Nil
        //todo fix class has same name
        (extCtx.filterNot(clz.descSketch.contains(_))).foreach {
          name => addItem(name, Nil,Nil)
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
   val name = new File(path).getName
    dotBuilder(sketch)
    println(graphContent)
    draw(name)
  }

}

