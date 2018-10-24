package muyan.diagramsketch

import scalariform.lexer.Token
import scalariform.parser.{AstNode, ParamClauses, Type}

sealed trait SketchNode {
  implicit def isExist[ T <: AstNode](para: Option[T]): Boolean = para.isDefined
  implicit def toOption[A <: SketchNode](in: A) : Option[A] = Some(in)
  implicit def isOption[A <: SketchNode](in: Option[A]) : Boolean = in.isDefined
  var deepLength = 0
}

case class ClazzSketch(marker: List[Token], name: Token) extends SketchNode
//todo scan inherit class para and class body info
case class InheritSketch(op: Option[List[Token]]) extends SketchNode

case class Slash(slh: Token) extends SketchNode

case class FunctionSketch(fun: List[Token], param: ParamClauses ,returnType: Option[(Token, Type)]) extends SketchNode


  case class DiagramLite(clz: ClazzSketch,
                       var ext: Option[InheritSketch] = None,
                       var inner: List[ClazzSketch] = Nil,
                       var fun: List[FunctionSketch] = Nil) extends SketchNode


