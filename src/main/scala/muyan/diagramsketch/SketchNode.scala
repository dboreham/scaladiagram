package muyan.diagramsketch

import scalariform.lexer.{Token, TokenType, Tokens}
import scalariform.parser.{AstNode, ParamClauses, Type}

sealed trait SketchNode {
  implicit def isExist[ T <: AstNode](para: Option[T]): Boolean = para.isDefined
  implicit def toOption[A <: SketchNode](in: A) : Option[A] = Some(in)
  implicit def isOption[A <: SketchNode](in: Option[A]) : Boolean = in.isDefined
  var deepLength = 0
  def descSketch: String
}

case class ClazzSketch(marker: List[Token], name: Token) extends SketchNode {
  override def descSketch: String = {
    name.rawText
  }
}
//todo scan inherit class para and class body info
//todo class define need modify for option nested with diagram lite
// return TemplateParents not List[Token]
case class InheritSketch(op: Option[List[Token]]) extends SketchNode {
  override def descSketch: String = {
   if(op.isDefined) op.get.map(_.text).mkString  else ""
  }
}

case class Slash(slh: Token) extends SketchNode {
  override def descSketch: String = ""
}

case class FunctionSketch(fun: List[Token], param: ParamClauses ,returnType: Option[(Token, Type)]) extends SketchNode {
  override def descSketch: String = {
    val name = fun.filter(_.tokenType.eq(Tokens.VARID)).map(_.text).mkString
    val paraList = param.tokens.map(_.text).mkString
    val ret =  if (returnType.isDefined)  {
      val v = returnType.get
      s"${v._1.text}${v._2.tokens.map(_.text).mkString}"
    } else ""

    s"$name$paraList$ret"
  }
}


  case class DiagramLite(clz: ClazzSketch,
                       var ext: Option[InheritSketch] = None,
                       var inner: List[ClazzSketch] = Nil,
                       var fun: List[FunctionSketch] = Nil) //extends SketchNode


