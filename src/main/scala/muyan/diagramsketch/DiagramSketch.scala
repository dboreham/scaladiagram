package muyan.diagramsketch

import java.nio.charset.MalformedInputException

import scala.language.implicitConversions
import scalariform.lexer.{ScalaLexer, Token, Tokens}
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
      case TmplDef(markerTokens, name, _, _, _, param, inherit : Option[TemplateInheritanceSection], body: Option[TemplateBody]) => {
        clzDeepLength += 1
       val pr = if (param) Some(eraseParamDefaultValues(param.get)) else None
       tempNode.append(updateTreeLength(ClazzSketch(markerTokens, name, pr), clzDeepLength))
       extractToken(inherit)
       extractToken(body)
        clzDeepLength -= 1
      }

      case  TemplateInheritanceSection(extend,_,parent) => {
        updateData{
          val inherit = if(!parent) Nil else {
            val value = parent.get
            //erase extends class generics type param
            var res = value.copy(typeAndArgs = (Type(value.typeAndArgs._1.contents.take(1)),value.typeAndArgs._2))
            val filter = res.withTypesAndArgs.map {
              case (token,tpe, args) => (token, Type(tpe.contents.take(1)), args)
            }
            res.copy(withTypesAndArgs = filter).tokens
          }
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
          val paraList = eraseParamDefaultValues(paramClauses)
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
      case PackageStat(_,pack) => {
      val name = pack.tokens.map(_.rawText).mkString
        tempNode.append(PackageSketch(name))
      }
      case _ => //println("### Not MATCH TYPE")
    }
  }

  
  private def updateData(f:  => SketchNode) : Unit = {
    clzDeepLength += 1
    tempNode.append(updateTreeLength(f, clzDeepLength))
    clzDeepLength -= 1
  }
 
   def eraseParamDefaultValues(paramClauses: ParamClauses) = {
    val filterVar: List[(ParamClause, Option[Token])] = paramClauses.paramClausesAndNewlines.map{
      case (pc, opt) =>
        if(pc.firstParamOption.isDefined) {
          val p: Param = pc.firstParamOption.get
          val firstVar =  p.copy(defaultValueOpt = None)
          val otherVar = pc.otherParams.map{
            case (token, param) =>
              (token,param.copy(defaultValueOpt = None))
          }
          (pc.copy(firstParamOption = Some(firstVar), otherParams = otherVar), opt)
        } else (pc, opt)
    }

    ParamClauses(paramClauses.newlineOpt, filterVar)
  }
 
  def updateTreeLength[B <: SketchNode](sn : B, len: Int): B = {
    sn.deepLength = len
    sn
  }


  def catalystSketch(path: String) = {

   val in = sourceParser(readFile(path, None))
   val stat = in.topStats.firstStatOpt :: in.topStats.otherStats.map(_._2)
   stat.foreach(extractToken(_))

     parseSketch(reconstructParseTree())
  }
  
 def reconstructParseTree() = {

    val len = tempNode.length
    var pack: PackageSketch = PackageSketch()
   
    val ret = new ListBuffer[ListBuffer[SketchNode]]()
    for( index <- 0 until len) {
      val elem = tempNode(index)
      
     if(elem.isInstanceOf[PackageSketch]) {
        pack = pack.copy(name = elem.asInstanceOf[PackageSketch].name)
      } else if (elem.isInstanceOf[ClazzSketch] ) {
       val r = getSameDeepNode(index, elem.deepLength)
        val p: ListBuffer[SketchNode] = getElement(r)
        p.append(pack) //add package name to the end of buffer
        ret.append(p.+=:(elem.asInstanceOf[ClazzSketch]))
      }

    }
    ret
  }

    def parseSketch(skh: ListBuffer[ListBuffer[SketchNode]])  = {
    val res = new ListBuffer[DiagramLite] //final result
    val itor = skh.toIterator

    while (itor.hasNext) {
      val in = itor.next()

      if(in.head.isInstanceOf[ClazzSketch]) {
        val head =  in.head.asInstanceOf[ClazzSketch]
        val elem = DiagramLite(head)
        for( el <- in  ) {
          val currentDeep = in.head.deepLength + 1
          val nextDeep = currentDeep + 1

          if (el.deepLength == currentDeep) {
            el  match {
              case inhert @ InheritSketch(_) => elem.ext = Some(inhert)
              case _ =>
            }
          } else if(el.deepLength == nextDeep) { //for class body sketch
            el match {
              case clz @ ClazzSketch(_, _, _) => elem.inner = clz :: elem.inner //inner class
              case fun @ FunctionSketch(_, _, _) => elem.method = fun :: elem.method //function get
              case attr @ AttributeSketch(_, _ ) => elem.attr = attr :: elem.attr
              case _ =>
            }
          } else if (el.deepLength.equals(0)) {
            elem.pack = el.asInstanceOf[PackageSketch]
          }
        }
        res.append(elem)
      }
    }
    res
  }

  
   //get element in temp node
  def getElement(range: ListBuffer[(Int, Int)]) = {
    range.map {
      case (a,b) if(b == a ) => {
        List(tempNode(a))
      }
      case (x,y) => {
        val start = tempNode.take(x)
        val end = tempNode.take(y)
        end.diff(start).toList
      }
    }.flatten
  }
 
    /**
    * when finding body and extend info, or getting class sketch, program must return result [range]
    * get body and extends elements with the same deep length 
    * */
  def getSameDeepNode(index: Int, deep: Int): ListBuffer[(Int,Int)] = {
    require((index >= 0 && index <= tempNode.length - 1), "index out of range in get same deeep node")
    var isLeftBrace = false
    var start = -1

    val range = new ListBuffer[(Int,Int)]()
    val scanStart = index + 1

    for ( i <- scanStart until tempNode.length if(scanStart < tempNode.length)) {
      val v = tempNode(i)

      if(v.deepLength.equals(deep + 1) ) {
        v match {
          case InheritSketch(_) => range.append((i, i))
          case Slash(lb) if( lb.tokenType.equals(Tokens.LBRACE)) => {
            start = i
            if (isLeftBrace) return range //repeat get the level lb
            isLeftBrace = true
          }
          case Slash(rb) if(isLeftBrace && rb.tokenType.equals(Tokens.RBRACE)) => {
            range.append((start, i))
            return range
          }
          case out if(out.isInstanceOf[ClazzSketch]  ) => {
            println("next level tree node, return")
            return range
          }
        }
      } else if (v.deepLength.equals(deep) && v.isInstanceOf[ClazzSketch]) {  
       return range
      }

    }
   range
  }
  
  
}

//end of class DiagramSketch


