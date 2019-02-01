package muyan.diagramsketch

import scalariform.lexer.{Token, Tokens}
import scalariform.parser.{ParamClauses, Type}

sealed trait SketchNode {
  var deepLength = 0
  def descSketch: List[String]
}

case class ClazzSketch(marker: List[Token], name: Token, paramClause: Option[ParamClauses] = None) extends SketchNode {
  override def descSketch: List[String] = {
    val pc: List[String] = if (paramClause.isDefined) {
      paramClause.get.paramClausesAndNewlines.map(_._1.tokens.map(_.rawText).mkString)
    } else Nil
    pc.+: (name.rawText)
  }
  
  def alias : String = name.rawText
}

// return TemplateParents not List[Token]
case class InheritSketch(op: List[Token]) extends SketchNode {
  override def descSketch: List[String] = {
//   if(op.isDefined) op.get.filter(_.tokenType.eq(Tokens.VARID)).map(_.text)  else Nil
   op.filter(_.tokenType.eq(Tokens.VARID)).map(_.text)
  }
}

case class Slash(slh: Token) extends SketchNode {
  override def descSketch: List[String] = Nil
}

case class FunctionSketch(method: List[Token], param: ParamClauses, returnType: Option[(Token, Type)]) extends SketchNode {
  override def descSketch: List[String] = {

    val name = method.filter(_.tokenType.eq(Tokens.VARID)).map(_.text).mkString
   // println(name)
    val paraList = param.tokens.map(_.text).mkString
    val ret =  if (returnType.isDefined)  {
      val v = returnType.get
      s"${v._1.text}${v._2.tokens.map(_.text).mkString}"
    } else ""

    List(s"$name$paraList$ret")
  }
}


case class AttributeSketch(name: List[Token], typeOpt: Option[List[Token]] = None) extends SketchNode {
  override def descSketch: List[String] = {
    val t = if(typeOpt.isDefined) ":" + typeOpt.get.map( _.text).mkString else ""
    val res = name.map(_.text + t)

    res
  }
}

case class PackageSketch(name: String = "") extends SketchNode {
  def descSketch: List[String] = List(name)
}
  case class DiagramLite(clz: ClazzSketch,
                         var ext: Option[InheritSketch] = None,
                         var inner: List[ClazzSketch] = Nil,
                         var method: List[FunctionSketch] = Nil,
                         var attr: List[AttributeSketch] = Nil,
                         var pack: PackageSketch = PackageSketch(),
                         var fileName: String = "") //extends SketchNode


