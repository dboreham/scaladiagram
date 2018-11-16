package muyan

import muyan.diagramsketch.{DiagramLite, DiagramSketch}
import muyan.graphviz.Graphviz
import muyan.template.DigraphBase
import muyan.template.EdgeGraph._

class Builder(path: String) {
  self: DiagramSketch with Graphviz with DigraphBase =>
  //todo check path is valid or not
  def isValid : Boolean = {
    //dir file
    true
  }

  val sketch =  catalystSketch(path)

  //todo parse function innerclass inherit class etc.
  //todo SketchNode add parsing function, every class override it.
  val dotBuilder =  sketch.foreach{ case DiagramLite(clz, ext, inner, method) =>
    addItem(clz.descSketch, method.map( e => s"${e.descSketch}").mkString("\n"),"")
    if(ext.isDefined) addRelation(clz.descSketch, ext.get.descSketch, inherit)
    inner.foreach {el =>
     addRelation(clz.descSketch, el.descSketch, composition)}
  }




}
