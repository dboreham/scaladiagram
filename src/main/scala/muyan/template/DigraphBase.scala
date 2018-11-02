  package muyan.template

import scala.collection.mutable.ListBuffer

 trait DigraphBase {
   sf: EdgeGraph =>
  val prefix = "digraph clz {"
  val postfix = "}"
  val node = s"""node [ fontname = "Courier New", fontsize = 13, shape = "record" ];"""
   val portName ="head"

  //class relation token, represent interface, generalization）, Realization，Association, Aggregation,
  // Composition, Dependency ...
//  val edge: String //mixed in digraph base

   //add an object to graph
  def addItem(name: String, fun: StringBuilder, varList: StringBuilder) = {
    val ret =  s"""$name [label = "{ <$portName> $name | <var> ${varList.toString()} | <fun> ${fun.toString()} }"];"""
    itemBuff.prepend(ret)
  }

  def addRelation(parent: String, child: String, relation: String) ={
   val t = s"$parent:$portName $relation $child:$portName"
    relationBuff.prepend(t)
  }
 private val itemBuff: ListBuffer[String] = new ListBuffer[String]
 private val relationBuff: ListBuffer[String] = new ListBuffer[String]

  final def graph: String = {
    require(itemBuff.nonEmpty )
    s"""$prefix
       |$node
       |${itemBuff.mkString("\n")}
       |${relationBuff.mkString("\n")}
       |$postfix """ .stripMargin
  }

}//end of digraphBase


