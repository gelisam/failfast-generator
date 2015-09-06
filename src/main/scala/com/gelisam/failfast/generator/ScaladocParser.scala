package com.gelisam.failfast.generator

import com.gelisam.parser.xml._

import scala.xml._
import scala.xml.{Elem => XmlElem}

case class Type(name: String) {
  override def toString: String =
    name
}

case class Parameter(name: String, parameterType: Type) {
  override def toString: String =
    s"${name}: ${parameterType}"
}

case class MethodSig(name: String, parameters: List[Parameter], returnType: Type) {
  override def toString: String =
    s"def ${name}(${parameters.mkString(", ")}): ${returnType}"
}

/**
 * Parses an html file into a list of method signatures.
 */
object ScaladocParser extends XmlParsers {
  val root = XML.loadFile("www.scala-lang.org/api/2.11.7/scala/collection/mutable/HashSet.html")
  val clearNode = (root \\ "li").filter(_ \@ "name" == "scala.collection.mutable.HashSet#clear").\("h4")(0)
  val foreachNode = (root \\ "li").filter(_ \@ "name" == "scala.collection.mutable.HashSet#foreach").\("h4")(0)
  
  /**
   * Parse an XML identifier into a String.
   */
  def identifierParser: Parser[String] =
    xmlElem("a").map(_.text)
  
  /**
   * Parse an XML type into a Type.
   */
  def typeParser: Parser[Type] =
    identifierParser.map(Type(_))
  
  /**
   * Parse an XML parameter into a Parameter.
   * So far, we only support one very specific case: (f: (A) => Unit).
   */
  def parameterParser: Parser[Parameter] =
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
      Parameter("f", Type("(A) => Unit"))
    }
  
  /**
   * Parse an XML parameter list into a List[Parameter].
   * So far, we only support two very specific case: () and (f: (A) => Unit).
   */
  def parametersParser: Parser[List[Parameter]] =
    (text("(") ~> parameterParser <~ text(")")).map(List(_)) |
    text("()").map(_ => List())
  
  /**
   * Parse an XML method signature into a MethodSig.
   * 
   * {{{
   * >>> ScaladocParser.parseAll(
   * ...   ScaladocParser.signatureParser,
   * ...   ScaladocParser.clearNode
   * ... ).get
   * def clear(): Unit
   * 
   * >>> ScaladocParser.parseAll(
   * ...   ScaladocParser.signatureParser,
   * ...   ScaladocParser.foreachNode
   * ... ).get
   * def foreach(f: (A) => Unit): Unit
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
            <PARAMETERS/>
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
      "PARAMETERS",
      parametersParser
    ).parseAs(
      "TYPE",
      typeParser
    ).map { case () ~ functionName ~ parameters ~ returnType =>
      MethodSig(functionName, parameters, returnType)
    }
}
