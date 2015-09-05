package com.gelisam.failfast.generator

import com.gelisam.parser.xml._

import treehugger.forest._, definitions._, treehuggerDSL._
import treehugger.forest._
import definitions._
import treehuggerDSL._

import scala.xml._
import scala.xml.{Elem => XmlElem}

case class Type(name: String)
case class MethodSig(name: String, parameters: List[(String, Type)], returnType: Type)

/**
 * Parses an html file into a list of method signatures.
 */
object ScaladocParser extends XmlParsers {
  val root = XML.loadFile("www.scala-lang.org/api/2.11.7/scala/collection/mutable/HashSet.html")
  val foreachNode = (root \\ "li").filter(_ \@ "name" == "scala.collection.mutable.HashSet#foreach")(0)
  
  /**
   * The signature of HashSet.foreach, encoded as XML.
   * 
   * {{{
   * >>> ScaladocParser.signatureNode
   * NodeSeq(<h4 class="signature">
   *       <span class="modifier_kind">
   *         <span class="modifier"/>
   *         <span class="kind">def</span>
   *       </span>
   *       <span class="symbol">
   *         <span class="name">foreach</span><span class="params">(<span name="f">f: (<span name="scala.collection.IterableLike.A" class="extype">A</span>) ⇒ <a name="scala.Unit" class="extype" href="../../Unit.html">Unit</a></span>)</span><span class="result">: <a name="scala.Unit" class="extype" href="../../Unit.html">Unit</a></span>
   *       </span>
   *       </h4>)
   */
  val signatureNode = (foreachNode \ "h4").filter(_ \@ "class" == "signature")
  
  /**
   * Parse an XML type into a Type.
   * So far, we only support the type Unit.
   * 
   * {{{
   * >>> ScaladocParser.parseAll(
   * ...   ScaladocParser.typeParser,
   * ...   (ScaladocParser.signatureNode \\ "a")(0)
   * ... ).get
   * Type(Unit)
   * }}}
   */
  def typeParser: Parser[Type] =
    XmlTemplate(
      <a name="scala.Unit" class="extype" href="../../Unit.html">
        Unit
      </a> 
    ).map {
      case () => Type("Unit")
    }
  
  def parameterParser: Parser[(String, Type)] =
    XmlTemplate(
      <span name="f">
        f: (
          <span name="scala.collection.IterableLike.A" class="extype">A</span>
        ) ⇒
        <a name="scala.Unit" class="extype" href="../../Unit.html">
          Unit
        </a>
      </span>
    ).map { case () =>
      ("f", Type("(A) => Unit"))
    }
  
  /**
   * Parse an XML method signature into a MethodSig.
   * 
   * {{{
   * >>> ScaladocParser.parseAll(
   * ...   ScaladocParser.signatureParser,
   * ...   ScaladocParser.signatureNode
   * ... ).get
   * MethodSig(foreach,List((f,Type((A) => Unit))),Type(Unit))
   * }}}
   */
  def signatureParser: Parser[MethodSig] =
    XmlTemplate(
      <h4 class="signature">
        <span class="modifier_kind">
          <span class="modifier"/>
          <span class="kind">def</span>
        </span>
        <span class="symbol">
          <span class="name"><FUNCTION_NAME/></span>
          <span class="params">
            (
              <PARAMETER/>
            )
          </span>
          <span class="result">
            :
            <TYPE/>
          </span>
        </span>
      </h4>
    ).parseAs(
      "FUNCTION_NAME",
      text
    ).parseAs(
      "PARAMETER",
      parameterParser
    ).parseAs(
      "TYPE",
      typeParser
    ).map { case () ~ function_name ~ parameter ~ return_type =>
      MethodSig(function_name, List(parameter), return_type)
    }
  
  /**
   * Parse an XML name into a treehugger Ident.
   * 
   * {{{
   * >>> ScaladocParser.parseAll(
   * ...   ScaladocParser.ident,
   * ...   <span class="name">foreach</span>
   * ... ).get
   * Ident(foreach)
   * }}}
   */
  def ident: Parser[Ident] =
    xmlElem("span").filter(_ \@ "class" == "name").map(x => Ident(x.text))
}
