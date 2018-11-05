package muyan.diagramsketch

import scalariform.lexer.{ScalaLexer, Token}
import scalariform.parser._

import scala.collection.mutable.ListBuffer
import scala.io.Source

//base code, can't get para if return type not explicit define
//todo path as constructor para
class DiagramSketch {

  val tempNode = new ListBuffer[SketchNode]()
//todo read file with code
  def readFile(path: String): String = {
    Source.fromFile(path).mkString
  }

  def sourceParser(source: String): CompilationUnit = {
    val tokens = ScalaLexer.tokenise(source, forgiveErrors = true, "2.11.0")
    new ScalaParser(tokens.toArray).compilationUnitOrScript()
  }

   implicit def isExist[ T <: AstNode](para: Option[T]): Boolean = para.isDefined
//   implicit def toOption[A <: SketchNode](in: A) : Option[A] = Some(in)
//   implicit def isOption[A <: SketchNode](in: Option[A]) : Boolean = in.isDefined

  var clzDeepLength = 0
  val lite = new ListBuffer[DiagramLite] //final result

  def extractToken(in: Option[AstNode]): Unit = {
    val stat = if(in) in.get else None

    stat match {
      case FullDefOrDcl(_, _, defOrDcl) => { //defOrDcl base class, not try to assign type
        extractToken(Some(defOrDcl))
      }
      case TmplDef(markerTokens, name, _, _, _, _, inherit : Option[TemplateInheritanceSection], body: Option[TemplateBody]) => {
        clzDeepLength += 1
       tempNode.append(updateTreeLength(ClazzSketch(markerTokens, name), clzDeepLength))
       extractToken(inherit)
       extractToken(body)
        clzDeepLength -= 1
      }
        //todo inherit para resolver
      case  TemplateInheritanceSection(extend,_,parent) => {

        clzDeepLength += 1
        val inherit = if(parent) Some(parent.get.tokens) else None
        tempNode.append(updateTreeLength(InheritSketch(inherit), clzDeepLength))
        clzDeepLength -= 1
      }
      case TemplateBody(_, lb, statSeq, rb) => {

        clzDeepLength += 1
        tempNode.append(updateTreeLength(Slash(lb), clzDeepLength ))
        statSeq.otherStats.foreach{el => extractToken(el._2)}
        extractToken(statSeq.firstStatOpt)
        tempNode.append(updateTreeLength(Slash(rb),clzDeepLength))
        clzDeepLength -= 1
      }
      case FunDefOrDcl(defToken, nameToken: Token, _, paramClauses, returnTypeOpt: Option[(Token, Type)], _, _) => {
        clzDeepLength += 1
        tempNode.append(updateTreeLength(FunctionSketch(List(defToken, nameToken), paramClauses, returnTypeOpt), clzDeepLength))
        clzDeepLength -= 1
      }
      case _ => //println("### Not MATCH TYPE")
    }
  }

  def updateTreeLength[B <: SketchNode](sn : B, len: Int): B = {
    sn.deepLength = len
    sn
  }

  def findRangeByDeepLength(deep: Int, range: ListBuffer[SketchNode]) = {

    var start = 0
    var target = 0
    val  res = new ListBuffer[Int]()
    while (target != range.length && target != -1) {
      target = range.indexWhere({
        x => x.deepLength == deep
      }, start)

      if (target != -1)  res.append(target)
      start = target + 1
    }

   res.append(range.size - 1)
   res.zip(res.tail).map {
     case (a,b) if(b - a == 1) => range.take(b).drop(a) //only has one element
     case (x,y) => range.splitAt(x)._2.splitAt(y-1)._1 //split a range list buffer
    }
  }

  def sketchClazz(deep: Int, in: ListBuffer[SketchNode]): Unit = {

    if(!in.head.isInstanceOf[ClazzSketch]) return
    val root =  in.head.asInstanceOf[ClazzSketch]
    val elem = DiagramLite(root)
    val currentDeep = deep
    val nextDeep = deep + 1

   //sketch body
    for( el <- in) {
      if (el.deepLength == currentDeep) {
       el  match {
          case inhert @ InheritSketch(_) => elem.ext = Some(inhert)
          case _ =>
        }
      }
      if(el.deepLength == nextDeep) { //for class body sketch
        el match {
          case clz @ ClazzSketch(_, _) => elem.inner = clz :: elem.inner //inner class
          case fun @ FunctionSketch(_, _, _) => elem.fun = fun :: elem.fun //function get
          case _ =>
        }
      }
    }
    //save lite to class val
    lite.append(elem)

    findRangeByDeepLength(nextDeep, in).foreach{
      nex => sketchClazz(nextDeep, nex)
    }
  }

  def catalystSketch(path: String) = {
   val in = sourceParser(readFile(path))
    in.topStats.otherStats.map(_._2).foreach(extractToken(_))
    //todo refactor findRangeByDeepLength function for single function
     findRangeByDeepLength(1, tempNode).foreach(t => sketchClazz(2, t))
    lite
  }
}

//end of class DiagramSketch


