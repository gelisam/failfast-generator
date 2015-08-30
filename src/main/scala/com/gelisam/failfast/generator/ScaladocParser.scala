package com.gelisam.failfast.generator

import com.gelisam.xml.parser._

import treehugger.forest._, definitions._, treehuggerDSL._
import treehugger.forest._
import definitions._
import treehuggerDSL._

import scala.xml._
import scala.xml.{Elem => XmlElem}

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
