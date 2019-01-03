package muyan.diagramsketch

import scalariform.lexer.{Token, Tokens}
import scalariform.parser.{ParamClauses, Type}

sealed trait SketchNode {
  var deepLength = 0
  def descSketch: List[String]
}

case class ClazzSketch(marker: List[Token], name: Token) extends SketchNode {
  override def descSketch: List[String] = {
    List(name.rawText)
  }
}
//todo scan inherit class para and class body info
//todo class define need modify for option nested with diagram lite
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

case class FunctionSketch(fun: List[Token], param: ParamClauses ,returnType: Option[(Token, Type)]) extends SketchNode {
  override def descSketch: List[String] = {
    println(fun)
    val name = fun.filter(_.tokenType.eq(Tokens.VARID)).map(_.text).mkString
    println(name)
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
   // println("AttributeSketch" + res)
    res
  }
}
  case class DiagramLite(clz: ClazzSketch,
                       var ext: Option[InheritSketch] = None,
                       var inner: List[ClazzSketch] = Nil,
                       var fun: List[FunctionSketch] = Nil,
                       var attr: List[AttributeSketch] = Nil) //extends SketchNode


