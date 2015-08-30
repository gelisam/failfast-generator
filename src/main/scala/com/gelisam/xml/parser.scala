package com.gelisam.xml.parser

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.xml._
import scala.xml.{Elem => XmlElem}

/**
 * An XML tree can be converted into a stream of XmlTokens by surrounding the
 * children of any element (even if they have zero children) by a pair of XmlOpen
 * and XmlClose tokens. Non-element nodes such as text nodes are simply wrapped
 * in an XmlNode token.
 * 
 * {{{
 * >>> val foo = <foo attr="ignored"/>
 * >>> val bar = <bar/>
 * >>> val both = <group>{foo} and {bar}</group>
 * >>> both
 * <group><foo attr="ignored"/> and <bar/></group>
 * 
 * // would be streamed as:
 * >>> List(
 * ...   XmlOpen(both),
 * ...     XmlOpen(foo),
 * ...     XmlClose(foo),
 * ...     XmlNode(new scala.xml.Text(" and ")),
 * ...     XmlOpen(bar),
 * ...     XmlClose(bar),
 * ...   XmlClose(both)
 * ... )
 * List(XmlOpen(<group><foo attr="ignored"/> and <bar/></group>), XmlOpen(<foo attr="ignored"/>), XmlClose(<foo attr="ignored"/>), XmlNode( and ), XmlOpen(<bar/>), XmlClose(<bar/>), XmlClose(<group><foo attr="ignored"/> and <bar/></group>))
 * }}}
 */
sealed trait XmlToken
case class XmlOpen(elem: Elem) extends XmlToken
case class XmlNode(node: Node) extends XmlToken
case class XmlClose(elem: Elem) extends XmlToken

/**
 * Convert a NodeSeq into a stream of XmlTokens.
 * 
 * {{{
 * >>> new XmlTokenReader(<group><foo attr="ignored"/> and <bar/></group>)
 * XmlTokenReader(XmlOpen(<group><foo attr="ignored"/> and <bar/></group>),XmlOpen(<foo attr="ignored"/>),XmlClose(<foo attr="ignored"/>),XmlNode( and ),XmlOpen(<bar/>),XmlClose(<bar/>),XmlClose(<group><foo attr="ignored"/> and <bar/></group>))
 * }}}
 */
class XmlTokenReader(
  nodeSeq: NodeSeq,
  index: Int = 0,
  cc: Option[(Elem, XmlTokenReader)] = None
) extends Reader[XmlToken] {
  def atEnd: Boolean =
    index == nodeSeq.length &&
    cc == None
  
  // helper for first and rest
  def nodeOption: Option[Node] =
    nodeSeq lift index
  
  def first: XmlToken =
    (nodeOption, cc) match {
      case (Some(elem: Elem), _) => XmlOpen(elem)
      case (Some(node), _) => XmlNode(node)
      case (None, Some((elem, _))) => XmlClose(elem)
      case (None, None) => throw new IllegalStateException("no more tokens")
    }
  
  // There is no position because our input is not a file.
  def pos: Position = NoPosition
  
  def rest: XmlTokenReader =
    (nodeOption, cc) match {
      case (Some(elem: Elem), cc) =>
        new XmlTokenReader(
          NodeSeq fromSeq (elem.child),
          0,
          Some((elem, new XmlTokenReader(nodeSeq, index + 1, cc)))
        )
      case (Some(_), cc) => new XmlTokenReader(nodeSeq, index + 1, cc)
      case (None, Some((_, cc))) => cc
      case (None, None) => throw new IllegalStateException("no more tokens")
    }
  
  def toList: List[XmlToken] =
    if (atEnd) Nil
    else first :: rest.toList
  
  // simplify debugging by listing all the remaining XmlTokens
  override def toString: String =
    s"XmlTokenReader(${toList mkString ","})"
}

/**
 * Parse XML using Scala's parser combinators.
 * 
 * Like the version at https://gist.github.com/pchiusano/822494, we flatten the
 * XML tree into a stream containing open and close tokens. Unlike that version,
 * we allow entire subtrees to be consumed at once, using NodeSeq's convenient
 * API for data extraction. This way, you can use the parser combinator API for
 * the parts which can change, and the NodeSeq API for the parts which don't.
 *
 * Like that version, the entire XML is kept in memory and thus the API isn't
 * suitable for streaming.
 * 
 * {{{
 * >>> XmlParsers.parsePrefix(XmlParsers.success(42), <foo/>).get
 * 42
 * }}}
 */
trait XmlParsers extends Parsers {
  type Elem = XmlToken
  
  /**
   * Succeeds if the parser matches the beginning of the input. Useful for debugging.
   * 
   * {{{
   * >>> XmlParsers.parsePrefix(
   * ...   XmlParsers.xmlElem ~ XmlParsers.xmlElem,
   * ...   <foo/><bar/>
   * ... ).get
   * (<foo/>~<bar/>)
   * }}}
   */
  def parsePrefix[A](parser: Parser[A], nodeSeq: NodeSeq): ParseResult[A] =
    parser(new XmlTokenReader(nodeSeq))
  
  /**
   * Succeeds if the parser matches the entire input.
   * 
   * {{{
   * >>> XmlParsers.parseAll(
   * ...   XmlParsers.xmlElem,
   * ...   <foo/><bar/>
   * ... )
   * [<undefined position>] failure: end of input expected
   * <BLANKLINE>
   * <undefined position>
   * 
   * >>> XmlParsers.parseAll(
   * ...   XmlParsers.xmlElem ~ XmlParsers.xmlElem,
   * ...   <foo/><bar/>
   * ... ).get
   * (<foo/>~<bar/>)
   * }}}
   */
  def parseAll[A](parser: Parser[A], nodeSeq: NodeSeq): ParseResult[A] =
    phrase(parser)(new XmlTokenReader(nodeSeq))
  
  /**
   * The next token, if there is one.
   * 
   * {{{
   * >>> XmlParsers.parsePrefix(
   * ...   XmlParsers.token ~ XmlParsers.token ~ XmlParsers.token,
   * ...   <text>hello</text>
   * ... ).get
   * ((XmlOpen(<text>hello</text>)~XmlNode(hello))~XmlClose(<text>hello</text>))
   * 
   * >>> XmlParsers.parsePrefix(
   * ...   XmlParsers.token ~ XmlParsers.token ~ XmlParsers.token ~ XmlParsers.token,
   * ...   <text>hello</text>
   * ... )
   * [<undefined position>] failure: end of input
   * <BLANKLINE>
   * <undefined position>
   * }}}
   */
  def token: Parser[XmlToken] =
    acceptIf(_ => true)(_ => "expected any token")
  
  /**
   * A text node, typically between other XML elements.
   * 
   * {{{
   * >>> XmlParsers.parseAll(
   * ...   XmlParsers.token ~> XmlParsers.text <~ XmlParsers.token,
   * ...   <text>hello</text>
   * ... ).get
   * hello
   * 
   * >>> XmlParsers.parseAll(
   * ...   XmlParsers.token ~> XmlParsers.text <~ XmlParsers.token,
   * ...   <text><hello/></text>
   * ... )
   * [<undefined position>] failure: text node expected
   * <BLANKLINE>
   * <undefined position>
   * }}}
   */
  def text: Parser[String] =
    accept("text node", {
      case XmlNode(scala.xml.Text(s)) => s
    })
  
  /**
   * A text node with the expected text.
   * 
   * {{{
   * >>> XmlParsers.parseAll(
   * ...   XmlParsers.token ~> XmlParsers.text("hello") <~ XmlParsers.token,
   * ...   <text>hello</text>
   * ... ).get
   * hello
   * 
   * >>> XmlParsers.parseAll(
   * ...   XmlParsers.token ~> XmlParsers.text("hello") <~ XmlParsers.token,
   * ...   <text>goodbye</text>
   * ... )
   * [<undefined position>] failure: hello expected
   * <BLANKLINE>
   * <undefined position>
   * }}}
   */
  def text(expected: String): Parser[String] =
    accept(expected, {
      case XmlNode(scala.xml.Text(s)) if s == expected => s
    })
  
  /**
   * A helper for the variants of xmlElem, parent and parentFlatMap which
   * accept any parent node.
   */
  private def open: Parser[XmlElem] =
    accept("opening tag", {
      case XmlOpen(elem) => elem
    })
  
  /**
   * A helper for the variants of xmlElem, parent and parentFlatMap which
   * require a parent node with a particular tag.
   */
  private def open(tag: String): Parser[XmlElem] =
    open ^? (
      {
        case elem if elem.label == tag => elem
      },
      {elem =>
        s"expected ${tag}, got ${elem.label}"
      }
    )
  
  /**
   * A helper for the variants of xmlElem, parent and parentFlatMap which
   * require a parent node with a particular tag and particular attributes.
   */
  private def open(expected: XmlElem): Parser[XmlElem] =
    {
      val expectedKeys = (expected.attributes map (_.key)).toSet
      expectedKeys.foldLeft(open(expected.label))((parser, key) =>
        parser ^? (
          {
            case elem if elem \@ key == expected \@ key => elem
          },
          {_ =>
            val expectedValue = expected \@ key
            s"""expected attribute ${key}="${expectedValue}""""
          }
        )
      ) ^? (
        {
          case elem if elem.attributes map (_.key) sameElements expectedKeys => elem
        },
        {elem =>
          val unexpectedAttributes = elem.attributes map (_.key)
          s"unexpected attribute ${unexpectedAttributes.head}"
        }
      )
    }
  
  /**
   * A helper for xmlElem, which accepts any children.
   */
  private def untilClose(openElem: XmlElem): Parser[XmlElem] =
    {
      lazy val parseUntilCloseTag: Parser[XmlElem] =
        token flatMap {
          case XmlClose(elem) if elem eq openElem => success(openElem)
          case _ => parseUntilCloseTag
        }
      parseUntilCloseTag
    }
  
  /**
   * A helper for parent, which requires children to satisfy a particular parser.
   */
  private def untilClose[A](openElem: XmlElem, parser: Parser[A]): Parser[A] =
    parser <~ accept("closing tag", {
      case XmlClose(elem) if elem eq openElem => elem
    })
  
  /**
   * A helper for parentFlatMap, which decides how to parse children using
   * monadic composition.
   */
  private def untilClose[A](openElem: XmlElem, f: XmlElem => Parser[A]): Parser[A] =
    untilClose(openElem, f(openElem))
  
  /**
   * An XML element with any tag name, any attributes, and any children.
   * 
   * {{{
   * >>> XmlParsers.parseAll(
   * ...   XmlParsers.parent("group", XmlParsers.xmlElem ~ XmlParsers.xmlElem),
   * ...   <group><foo attr="ignored"/><bar/></group>
   * ... ).get
   * (<foo attr="ignored"/>~<bar/>)
   * 
   * >>> XmlParsers.parseAll(
   * ...   XmlParsers.parent("group", XmlParsers.xmlElem ~ XmlParsers.xmlElem),
   * ...   <group><foo attr="ignored"/> and <bar/></group>
   * ... )
   * [<undefined position>] failure: opening tag expected
   * <BLANKLINE>
   * <undefined position>
   * }}}
   */
  def xmlElem: Parser[XmlElem] =
    open flatMap (untilClose(_))
  
  /**
   * An XML element with the given tag name, any attributes, and any children.
   * 
   * {{{
   * >>> XmlParsers.parseAll(
   * ...   XmlParsers.xmlElem("foo") ~ XmlParsers.xmlElem("bar"),
   * ...   <foo attr="ignored"/><bar/>
   * ... ).get
   * (<foo attr="ignored"/>~<bar/>)
   * 
   * >>> XmlParsers.parseAll(
   * ...   XmlParsers.xmlElem("foo") ~ XmlParsers.xmlElem("bar"),
   * ...   <foo/><foo/>
   * ... )
   * [<undefined position>] failure: expected bar, got foo
   * <BLANKLINE>
   * <undefined position>
   * }}}
   * 
   * >>> XmlParsers.parseAll(
   * ...   XmlParsers.xmlElem("foo") ~ XmlParsers.xmlElem("bar"),
   * ...   <bar/><bar/>
   * ... )
   * [<undefined position>] failure: expected foo, got bar
   * <BLANKLINE>
   * <undefined position>
   * }}}
   */
  def xmlElem(tag: String): Parser[XmlElem] =
    open(tag) flatMap (untilClose(_))
  
  /**
   * An XML element with the given tag name, the given attributes, and any children.
   * 
   * {{{
   * >>> XmlParsers.parseAll(
   * ...   XmlParsers.xmlElem(<foo attr="matters"/>),
   * ...   <foo attr="matters"/>
   * ... ).get
   * <foo attr="matters"/>
   * }}}
   * 
   * >>> XmlParsers.parseAll(
   * ...   XmlParsers.xmlElem(<foo attr="matters"/>),
   * ...   <foo/>
   * ... )
   * [<undefined position>] failure: expected attribute attr="matters"
   * <BLANKLINE>
   * <undefined position>
   * 
   * >>> XmlParsers.parseAll(
   * ...   XmlParsers.xmlElem(<foo/>),
   * ...   <foo attr="matters"/><foo/>
   * ... )
   * [<undefined position>] failure: unexpected attribute attr
   * <BLANKLINE>
   * <undefined position>
   */
  def xmlElem(expected: XmlElem): Parser[XmlElem] =
    open(expected) flatMap (untilClose(_))
  
  /**
   * An XML element, with any tag name and any attributes, whose children match
   * the given parser.
   * 
   * {{{
   * >>> XmlParsers.parseAll(
   * ...   XmlParsers.parent(XmlParsers.text.map(_.toInt)),
   * ...   <number>42</number>
   * ... ).get
   * 42
   * }}}
   */
  def parent[A](parser: Parser[A]): Parser[A] =
    open flatMap (untilClose(_, parser))
  
  /**
   * An XML element, with the given tag name and any attributes, whose children
   * match the given parser.
   * 
   * {{{
   * >>> XmlParsers.parseAll(
   * ...   XmlParsers.parent("number", XmlParsers.text.map(_.toInt)),
   * ...   <number>42</number>
   * ... ).get
   * 42
   * }}}
   */
  def parent[A](tag: String, parser: Parser[A]): Parser[A] =
    open(tag) flatMap (untilClose(_, parser))
  
  /**
   * An XML element, with any tag name and any attributes, which can be examined
   * as a DOM to determine how to parse its children. Useful when the contents
   * of an element is described by its attributes.
   * 
   * {{{
   * >>> XmlParsers.parseAll(
   * ...   XmlParsers.parentFlatMap(elem =>
   * ...     XmlParsers.repN(
   * ...       (elem \@ "count").toInt,
   * ...       XmlParsers.parent("entry", XmlParsers.text)
   * ...     )
   * ...   ),
   * ...   <list count="2"><entry>hello</entry><entry>world</entry></list>
   * ... ).get
   * List(hello, world)
   * }}}
   */
  def parentFlatMap[A](f: XmlElem => Parser[A]): Parser[A] =
    open flatMap (untilClose(_,f))
  
  /**
   * An XML element, with the given tag name and any attributes, which can be
   * examined as a DOM to determine how to parse its children. Useful when the
   * contents of an element is described by its attributes.
   * 
   * {{{
   * >>> XmlParsers.parseAll(
   * ...   XmlParsers.parentFlatMap("list")(elem =>
   * ...     XmlParsers.repN(
   * ...       (elem \@ "count").toInt,
   * ...       XmlParsers.parent("entry", XmlParsers.text)
   * ...     )
   * ...   ),
   * ...   <list count="2"><entry>hello</entry><entry>world</entry></list>
   * ... ).get
   * List(hello, world)
   * }}}
   */
  def parentFlatMap[A](tag: String)(f: XmlElem => Parser[A]): Parser[A] =
    open(tag) flatMap (untilClose(_,f))
  
  /**
   * An XML element, with the given tag name and attributes, which can be
   * examined as a DOM to determine how to parse its children. Useful when the
   * contents of an element is described by its attributes.
   * 
   * {{{
   * >>> XmlParsers.parseAll(
   * ...   XmlParsers.parentFlatMap(<list count="inline"/>) { elem =>
   * ...     val count = (elem \ "count").text.toInt
   * ...     val ignoredCount = XmlParsers.xmlElem("count").?
   * ...     val entry = XmlParsers.parent("entry", XmlParsers.text)
   * ...     ignoredCount ~> XmlParsers.repN(count, entry <~ ignoredCount)
   * ...   },
   * ...   <list count="inline"><entry>hello</entry><entry>world</entry><count>2</count></list>
   * ... ).get
   * List(hello, world)
   * }}}
   */
  def parentFlatMap[A](expected: XmlElem)(f: XmlElem => Parser[A]): Parser[A] =
    open(expected) flatMap (untilClose(_,f))
}

object XmlParsers extends XmlParsers
