  package muyan.template

import scala.collection.mutable.ListBuffer

 trait DigraphBase {
  val prefix = "digraph clz {"
  val postfix = "}"
  val node = s"""node [ fontname = "Courier New", fontsize = 13, shape = "record", color="brown1"  ];"""
   val portName ="head"
   val methodName ="fun"
   val rankDir = "rankdir = BT"

  //class relation token, represent interface, generalization）, Realization，Association, Aggregation,
  // Composition, Dependency ...
//  val edge: String //mixed in digraph base

  def addItem(name: String, fun: List[String], varList: List[String]) = {
    
   def toStringWithEscape(t: List[String]): String = t.map(x => x + "\\l").mkString
    
    val ret =  s"""$name [label = "{ <$portName> $name | <var> ${toStringWithEscape(varList)} | <fun> ${toStringWithEscape(fun)} }"];"""
    itemBuff.prepend(ret.replace("=>", "=\\>"))
  }

  def addRelation(parent: String, child: String, relation: String) ={
    val t = s"$parent:$portName -> $child:$methodName $relation"
    relationBuff.prepend(t)

  }

 private val itemBuff: ListBuffer[String] = new ListBuffer[String]
 private val relationBuff: ListBuffer[String] = new ListBuffer[String]

  final def graphContent: String = {
    require(itemBuff.nonEmpty )
    s"""$prefix
       $rankDir
       |$node
       |${itemBuff}
       |${relationBuff}
       |$postfix """ .stripMargin
  }

}//end of digraphBase


