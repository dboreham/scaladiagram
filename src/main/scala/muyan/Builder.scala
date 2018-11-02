package muyan

import muyan.diagramsketch.DiagramSketch
import muyan.graphviz.Graphviz
import muyan.template.{DigraphBase, EdgeGraph}

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
  val dotBuilder =  sketch.foreach{ el =>
   val name = el.clz.name.rawText

  }



}
