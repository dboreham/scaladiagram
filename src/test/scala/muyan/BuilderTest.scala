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

   val path = "d:\"
    val builder = new Builder(path,None, "pdf")

    builder.build

  }


}
