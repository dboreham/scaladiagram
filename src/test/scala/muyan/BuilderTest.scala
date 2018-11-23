package muyan

import java.util.ResourceBundle

import muyan.diagramsketch.{DiagramSketch, FunctionSketch}
import muyan.graphviz.Graphviz
import muyan.template.DigraphBase
import org.scalatest.{FlatSpec, ShouldMatchers}
import scalariform.lexer.Token
import scalariform.parser.ParamClauses

import scala.util.Try

class BuilderTest extends FlatSpec with ShouldMatchers  {

  "muyan" should "test Builder function about dot string and sketch result" in {
    val path = "M:\\BIGDATA\\SourceCode\\wayne\\src\\main\\scala\\com\\wayne\\future\\PerformManagement.scala"
//    val path = "M:\\BIGDATA\\SourceCode\\scaladiagram\\src\\main\\scala\\muyan\\Builder.scala"
    val builder = new Builder(path)

    builder.build

//    val in = builder.sourceParser(builder.readFile(path))
//    //    in.topStats.otherStats.map(_._2).foreach(extractToken(_))
//    in.topStats.otherStats.foreach(x => builder.extractToken(x._2))
//    builder.fileSketch.foreach {
//      el =>
//        println("* " + el.clz)
//        println("[]"+ el.clz.descSketch)
//        println("--------------")
//        println("* " + el.ext)
//        if (el.ext.isDefined) println(el.ext.get.descSketch)

//        println("--------------")
//        println("* " + el.inner)
//        println("[]"+ el.inner.foreach(_.descSketch))
//        println("--------------")
//        println("* " + el.fun)
//        println("[]"+ el.fun.foreach(_.descSketch))
//        println("--------------")
//    }


  }


}
