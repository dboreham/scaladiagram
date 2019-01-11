package muyan.diagramsketch

import java.nio.charset.MalformedInputException
import scala.language.implicitConversions
import scalariform.lexer.{ScalaLexer, Token}
import scalariform.parser._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.{Codec, Source}


trait DiagramSketch {

  //temp node info in a file or package
  val tempNode = new ListBuffer[SketchNode]()

def readFile(file: String, encoding: Option[String])(implicit codec: Codec): String = {
  @tailrec //tailrec for reading encoding
  def readFileWithEncoding(file: String, encodings: List[String]): Option[String] = {
    if (encodings.isEmpty) {
      None
    } else {
      val encoding = encodings.head
      try {
        Some(Source.fromFile(file)(encoding).mkString)
      } catch {
        case _: MalformedInputException =>
          readFileWithEncoding(file, encodings.tail)
      }
    }
  }

  val encodings = encoding match {
    case Some(x) => List(x)
    case None => List(codec.charSet.toString, "UTF-8", "UTF-16", "ISO-8859-1")
  }

  // as far as I can tell, most files should be readable with ISO-8859-1 (though obviously it won't
  // return the correct characters), so I don't know under what circumstances we can get
  // the MalformedInputException (and therefore) RuntimeException here.
  readFileWithEncoding(file, encodings) match {
    case None => throw new RuntimeException("Could not read file, caught MalformedInputException")
    case Some(source) => source
  }
}


  def sourceParser(source: String): CompilationUnit = {
    val tokens = ScalaLexer.tokenise(source, forgiveErrors = true, "2.11.0")
    new ScalaParser(tokens.toArray).compilationUnitOrScript()
  }

   implicit def isExist[ T <: AstNode](para: Option[T]): Boolean = para.isDefined

    /**
    * For recursion sketching scala classes, results should be saved like binary tree.
    * root is class ,leafs are extend class and method. @clzDeepLength describes the relation.
    * */
  private var clzDeepLength = 0
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

      case  TemplateInheritanceSection(extend,_,parent) => {
        updateData{
          val inherit = if(parent) parent.get.tokens else Nil
          InheritSketch(inherit)
        }
      }
      case TemplateBody(_, lb, statSeq, rb) => {
        updateData{
          tempNode.append(updateTreeLength(Slash(lb), clzDeepLength ))
          statSeq.otherStats.foreach{el => extractToken(el._2)}
          extractToken(statSeq.firstStatOpt)
          Slash(rb)
        }

      }
      case FunDefOrDcl(defToken, nameToken: Token, _, paramClauses, returnTypeOpt: Option[(Token, Type)], _, _) => {
        updateData{
         val filterVar: List[(ParamClause, Option[Token])] = paramClauses.paramClausesAndNewlines.map{
                case (pc, opt) =>
                  if(pc.firstParamOption.isDefined) {
                    val p: Param = pc.firstParamOption.get
                    val setVar =  p.copy(defaultValueOpt = None)
                    (pc.copy(firstParamOption = Some(setVar)), opt)
                  } else (pc, opt)
              }
          
          val paraList = ParamClauses(paramClauses.newlineOpt, filterVar)
          FunctionSketch(List(defToken, nameToken), paraList, returnTypeOpt)
        }

      }
      case PatDefOrDcl(_, expr, otherPatterns, typedOpt, _) => { //sketch variable param
        updateData{
          val attr = expr.tokens ::: otherPatterns.flatMap(_._2.tokens)
          val t =  if(typedOpt.isDefined) Some(typedOpt.get._2.tokens) else  None
          AttributeSketch(attr, t)
        }
      }
      case _ => //println("### Not MATCH TYPE")
    }
  }

  
  private def updateData(f:  => SketchNode) : Unit = {
    clzDeepLength += 1
    tempNode.append(updateTreeLength(f, clzDeepLength))
    clzDeepLength -= 1
  }
  
  def updateTreeLength[B <: SketchNode](sn : B, len: Int): B = {
    sn.deepLength = len
    sn
  }

  /**
    *Given a deep length, split @range the same node as deep
    * example: deep =1 means finding parent class text range
    *@param deep  deep length about buffer
    *@param range giving list
   */
  def findSameClazzByDeepLength(deep: Int, range: ListBuffer[SketchNode]) = {

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
     //only has one element
     case (a,b) if(b == a ) => {
    val t = if(b == 0) range.take(1) else range.take(b).drop(b - 1)
//       println(s"b:$b-a:$a:" + t)
       t
     }
     case (x,y) => {
         val start = range.take(x)
         val end = range.take(y)
         end.diff(start)
     }
    }
  }

  /**
    * @define sketch one class after class token being parsed
    * @param root class deep length
    * @param in nodes in a class including parent/children class, innerclass
    *           and function etc.
    * */
  def sketchClazz(root: Int, in: ListBuffer[SketchNode]): Unit = {

    if(!in.head.isInstanceOf[ClazzSketch]) return
    val head =  in.head.asInstanceOf[ClazzSketch]
    val elem = DiagramLite(head)
    val currentDeep = root
    val nextDeep: Int = root + 1

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
          case fun @ FunctionSketch(_, _, _) => elem.method = fun :: elem.method //function get
          case attr @ AttributeSketch(_, _ ) => elem.attr = attr :: elem.attr
          case _ =>
        }
      }
    }
    //save lite to class val
    lite.append(elem)

    findSameClazzByDeepLength(nextDeep, in).foreach{
      nex => sketchClazz(nextDeep, nex)
    }
  }

  def catalystSketch(path: String) = {

   val in = sourceParser(readFile(path, None))
//    in.topStats.otherStats.map(_._2).foreach(extractToken(_))
    in.topStats.otherStats.foreach(x => extractToken(x._2))

     findSameClazzByDeepLength(1, tempNode).foreach(t => sketchClazz(2, t))
    lite
  }
}

//end of class DiagramSketch


