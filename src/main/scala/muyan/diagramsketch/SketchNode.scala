package muyan.diagramsketch

import scalariform.lexer.{Token, TokenType, Tokens}
import scalariform.parser.{AstNode, ParamClauses, Type}

sealed trait SketchNode {
  implicit def isExist[ T <: AstNode](para: Option[T]): Boolean = para.isDefined
  implicit def toOption[A <: SketchNode](in: A) : Option[A] = Some(in)
  implicit def isOption[A <: SketchNode](in: Option[A]) : Boolean = in.isDefined
  var deepLength = 0
  def descSketch: List[String]
}

case class ClazzSketch(marker: List[Token], name: Token) extends SketchNode {
  override def descSketch: List[String] = {
//    val prefix = marker.map(_.rawText).mkString(" ")
    List(name.rawText)
  }
}
//todo scan inherit class para and class body info
case class InheritSketch(op: Option[List[Token]]) extends SketchNode {
  override def descSketch: List[String] = {

   if(op.isDefined) op.get.map(_.text)  else Nil
  }
}

case class Slash(slh: Token) extends SketchNode {
  override def descSketch: List[String] = Nil
}

case class FunctionSketch(fun: List[Token], param: ParamClauses ,returnType: Option[(Token, Type)]) extends SketchNode {
  override def descSketch: List[String] = {
    val name = fun.filter(_.tokenType.eq(Tokens.VARID)).map(_.text).mkString
    List(name)
  }
}


  case class DiagramLite(clz: ClazzSketch,
                       var ext: Option[InheritSketch] = None,
                       var inner: List[ClazzSketch] = Nil,
                       var fun: List[FunctionSketch] = Nil) //extends SketchNode





