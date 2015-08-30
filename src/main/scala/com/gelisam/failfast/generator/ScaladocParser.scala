package com.gelisam.failfast.generator

import com.gelisam.xml.parser._

import treehugger.forest._, definitions._, treehuggerDSL._
import treehugger.forest._
import definitions._
import treehuggerDSL._

import scala.xml._

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
   * Parse an XML name into a String.
   * 
   * {{{
   * >>> ScaladocParser.parseName(<span class="name">foreach</span>)
   * foreach
   * }}}
   */
  def parseName(elem: scala.xml.Elem): String =
    elem.text
  
  /**
   * Parse an XML name into a treehugger Ident.
   * 
   * {{{
   * >>> ScaladocParser.parseIdent(<span class="name">foreach</span>)
   * Ident(foreach)
   * }}}
   */
  def parseIdent(elem: scala.xml.Elem): Ident =
    Ident(parseName(elem))
}
