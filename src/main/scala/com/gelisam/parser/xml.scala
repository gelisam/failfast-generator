package com.gelisam.parser.xml

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
 * >>> val foo = <foo attr="value"/>
 * >>> val bar = <bar/>
 * >>> val both = <group>{foo} and {bar}</group>
 * >>> both
 * <group><foo attr="value"/> and <bar/></group>
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
 * List(XmlOpen(<group><foo attr="value"/> and <bar/></group>), XmlOpen(<foo attr="value"/>), XmlClose(<foo attr="value"/>), XmlNode( and ), XmlOpen(<bar/>), XmlClose(<bar/>), XmlClose(<group><foo attr="value"/> and <bar/></group>))
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
 * >>> XmlTokenReader(<group><foo attr="value"/> and <bar/></group>)
 * XmlTokenReader(XmlOpen(<group><foo attr="value"/> and <bar/></group>),XmlOpen(<foo attr="value"/>),XmlClose(<foo attr="value"/>),XmlNode( and ),XmlOpen(<bar/>),XmlClose(<bar/>),XmlClose(<group><foo attr="value"/> and <bar/></group>))
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

object XmlTokenReader {
  def apply(nodeSeq: NodeSeq): XmlTokenReader =
    new XmlTokenReader(nodeSeq)
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
 * >>> import XmlParsers._
 * >>> parsePrefix(success(42), <foo/>).get
 * 42
 * }}}
 */
trait XmlParsers extends Parsers {
  type Elem = XmlToken
  
  /**
   * Succeeds if the parser matches the beginning of the input. Useful for debugging.
   * 
   * {{{
   * >>> import XmlParsers._
   * >>> parsePrefix(
   * ...   xmlElem ~ xmlElem,
   * ...   <foo/><bar/>
   * ... ).get
   * (<foo/>~<bar/>)
   * }}}
   */
  def parsePrefix[A](parser: Parser[A], nodeSeq: NodeSeq): ParseResult[A] =
    parser(XmlTokenReader(nodeSeq))
  
  /**
   * Succeeds if the parser matches the entire input.
   * 
   * {{{
   * >>> import XmlParsers._
   * >>> parseAll(
   * ...   xmlElem,
   * ...   <foo/><bar/>
   * ... )
   * [<undefined position>] failure: end of input expected
   * <BLANKLINE>
   * <undefined position>
   * 
   * >>> parseAll(
   * ...   xmlElem ~ xmlElem,
   * ...   <foo/><bar/>
   * ... ).get
   * (<foo/>~<bar/>)
   * }}}
   */
  def parseAll[A](parser: Parser[A], nodeSeq: NodeSeq): ParseResult[A] =
    phrase(parser)(XmlTokenReader(nodeSeq))
  
  /**
   * The next token, if there is one.
   * 
   * {{{
   * >>> import XmlParsers._
   * >>> parsePrefix(
   * ...   token ~ token ~ token,
   * ...   <text>hello</text>
   * ... ).get
   * ((XmlOpen(<text>hello</text>)~XmlNode(hello))~XmlClose(<text>hello</text>))
   * 
   * >>> parsePrefix(
   * ...   token ~ token ~ token ~ token,
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
   * >>> import XmlParsers._
   * >>> parseAll(
   * ...   token ~> text <~ token,
   * ...   <text>hello</text>
   * ... ).get
   * hello
   * 
   * >>> parseAll(
   * ...   token ~> text <~ token,
   * ...   <text><hello/></text>
   * ... )
   * [<undefined position>] failure: text node expected
   * <BLANKLINE>
   * <undefined position>
   * }}}
   */
  def text: Parser[String] =
    accept("text node", {
      case XmlNode(Text(s)) => s
    })
  
  /**
   * A text node with the expected text.
   * 
   * {{{
   * >>> import XmlParsers._
   * >>> parseAll(
   * ...   token ~> text("hello") <~ token,
   * ...   <text>hello</text>
   * ... ).get
   * hello
   * 
   * >>> parseAll(
   * ...   token ~> text("hello") <~ token,
   * ...   <text>goodbye</text>
   * ... )
   * [<undefined position>] failure: hello expected
   * <BLANKLINE>
   * <undefined position>
   * }}}
   */
  def text(expected: String): Parser[String] =
    accept(expected, {
      case XmlNode(Text(s)) if s == expected => s
    })
  
  /**
   * A helper for the variants of xmlElem, parent and parentFlatMap which
   * accept any parent node.
   */
  private[parser] def open: Parser[XmlElem] =
    accept("opening tag", {
      case XmlOpen(elem) => elem
    })
  
  /**
   * A helper for the variants of xmlElem, parent and parentFlatMap which
   * require a parent node with a particular tag.
   */
  private[parser] def open(tag: String): Parser[XmlElem] =
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
  private[parser] def open(expected: XmlElem): Parser[XmlElem] =
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
  private[parser] def untilClose(openElem: XmlElem): Parser[XmlElem] =
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
  private[parser] def untilClose[A](openElem: XmlElem, parser: Parser[A]): Parser[A] =
    parser <~ accept("closing tag", {
      case XmlClose(elem) if elem eq openElem => elem
    })
  
  /**
   * A helper for parentFlatMap, which decides how to parse children using
   * monadic composition.
   */
  private[parser] def untilClose[A](openElem: XmlElem, f: XmlElem => Parser[A]): Parser[A] =
    untilClose(openElem, f(openElem))
  
  /**
   * An XML element with any tag name, any attributes, and any children.
   * 
   * {{{
   * >>> import XmlParsers._
   * >>> parseAll(
   * ...   parent("group", xmlElem ~ xmlElem),
   * ...   <group><foo attr="ignored"/><bar/></group>
   * ... ).get
   * (<foo attr="ignored"/>~<bar/>)
   * 
   * >>> parseAll(
   * ...   parent("group", xmlElem ~ xmlElem),
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
   * >>> import XmlParsers._
   * >>> parseAll(
   * ...   xmlElem("foo") ~ xmlElem("bar"),
   * ...   <foo attr="ignored"/><bar/>
   * ... ).get
   * (<foo attr="ignored"/>~<bar/>)
   * 
   * >>> parseAll(
   * ...   xmlElem("foo") ~ xmlElem("bar"),
   * ...   <foo/><foo/>
   * ... )
   * [<undefined position>] failure: expected bar, got foo
   * <BLANKLINE>
   * <undefined position>
   * }}}
   * 
   * >>> parseAll(
   * ...   xmlElem("foo") ~ xmlElem("bar"),
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
   * >>> import XmlParsers._
   * >>> parseAll(
   * ...   xmlElem(<foo attr="value"/>),
   * ...   <foo attr="value"/>
   * ... ).get
   * <foo attr="value"/>
   * }}}
   * 
   * >>> parseAll(
   * ...   xmlElem(<foo attr="value"/>),
   * ...   <foo/>
   * ... )
   * [<undefined position>] failure: expected attribute attr="value"
   * <BLANKLINE>
   * <undefined position>
   * 
   * >>> parseAll(
   * ...   xmlElem(<foo/>),
   * ...   <foo attr="value"/><foo/>
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
   * >>> import XmlParsers._
   * >>> parseAll(
   * ...   parent(text.map(_.toInt)),
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
   * >>> import XmlParsers._
   * >>> parseAll(
   * ...   parent("number", text.map(_.toInt)),
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
   * >>> import XmlParsers._
   * >>> parseAll(
   * ...   parentFlatMap(elem =>
   * ...     repN(
   * ...       (elem \@ "count").toInt,
   * ...       parent("entry", text)
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
   * >>> import XmlParsers._
   * >>> parseAll(
   * ...   parentFlatMap("list")(elem =>
   * ...     repN(
   * ...       (elem \@ "count").toInt,
   * ...       parent("entry", text)
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
   * >>> import XmlParsers._
   * >>> parseAll(
   * ...   parentFlatMap(<list count="inline"/>) { elem =>
   * ...     val count = (elem \ "count").text.toInt
   * ...     val ignoredCount = xmlElem("count").?
   * ...     val entry = parent("entry", text)
   * ...     ignoredCount ~> repN(count, entry <~ ignoredCount)
   * ...   },
   * ...   <list count="inline"><entry>hello</entry><entry>world</entry><count>2</count></list>
   * ... ).get
   * List(hello, world)
   * }}}
   */
  def parentFlatMap[A](expected: XmlElem)(f: XmlElem => Parser[A]): Parser[A] =
    open(expected) flatMap (untilClose(_,f))
  
  /**
   * This exact sequence of XML nodes, as is.
   * 
   * {{{
   * >>> import XmlParsers._
   * >>> parseAll(
   * ...   nodeSeq(<group><foo/>hello<bar/></group>),
   * ...   <group><foo/>hello<bar/></group>
   * ... ).get
   * <group><foo/>hello<bar/></group>
   * 
   * >>> parseAll(
   * ...   nodeSeq(<foo/>),
   * ...   <bar/>
   * ... )
   * [<undefined position>] failure: `XmlOpen(<foo/>)' expected but XmlOpen(<bar/>) found
   * <BLANKLINE>
   * <undefined position>
   * }}}
   */
  def nodeSeq(nodeSeq: NodeSeq): Parser[NodeSeq] =
    XmlTokenReader(nodeSeq).toList.map(elem(_)).foldLeft(success(nodeSeq))(_ <~ _)
  
  /**
   * Now for the complicated part.
   * 
   * Scala has a built-in syntax for describing a rigid block of XML surrounding
   * more malleable pieces of data:
   * 
   * {{{
   * >>> import XmlParsers._
   * >>> val myInt = 42
   * >>> val myFloat = 1.5
   * >>> <group><i>{myInt}</i><f>{myFloat}</f></group>
   * <group><i>42</i><f>1.5</f></group>
   * }}}
   * 
   * I wish I could use that same syntax for describing a parser expecting that
   * same rigid block of XML, surrounding more malleable pieces described by
   * sub-parsers.
   * 
   * {{{
   * >>> val intParser: Parser[Int] = text.map(_.toInt)
   * ... val floatParser: Parser[Float] = text.map(_.toFloat)
   * 
   * ... val groupParser: Parser[Float] =
   * ...   XmlTemplate(
   * ...     <group><i>{intParser}</i><f>{floatParser}</f></group>
   * ...   ) map {
   * ...     case (i, f) => i + f
   * ...   }
   * ... parseAll(
   * ...   groupParser,
   * ...   <group><i>42</i><f>1.5</f></group>
   * ... ).get
   * 43.5
   * }}}
   * 
   * Unfortunately, the types of intParser and floatParser are lost during the
   * XML conversion, so there would be no way to type check the call to map.
   * 
   * Here's an alternative, slightly less convenient syntax which nevertheless
   * preserves the clarity of the rigid block of XML, and for which type checking
   * is possible:
   * 
   * {{{
   * >>> val groupParser: Parser[Float] =
   * ...   XmlTemplate(
   * ...     <group><i><int/></i><f><float/></f></group>
   * ...   ).parseAs(
   * ...     "int", intParser
   * ...   ).parseAs(
   * ...     "float", floatParser
   * ...   ) map {
   * ...     case (f, (i, ())) => i + f
   * ...   }
   * >>> parseAll(
   * ...   groupParser,
   * ...   <group><i>42</i><f>1.5</f></group>
   * ... ).get
   * 43.5
   * }}}
   * 
   * Implementation-wise, this API is a bit tricky to achieve, because:
   * 
   * 1. We can't compose the rigid parsers until we know which of them won't be
   *    replaced by malleable parsers.
   * 2. This in turn prevents us from composing the malleable parsers, because
   *    there might be a rigid-until-further-notice leaf parser in between them.
   * 3. The order in which the parser names appear in the XML might differ from
   *    the order in which the parser names are replaced. We would like to return
   *    the tuple of results in the latter order.
   * 
   * Here is my solution. Instead of keeping track of the parsers, I keep and
   * manipulate a term representing the steps required to build the combined
   * parser. To replace a rigid parser with a malleable parser, I find the step
   * which adds the rigid parser and ignores is result, I replace it with the
   * malleable parser, and I modify the later steps to thread the result along.
   * To change the order of the final tuple, I simply add steps which reorder the
   * results.
   */
  object XmlTemplate {
    // In Haskell, Term and parseAs would look like this:
    // 
    // data Term a where
    //   Nil :: Term ()
    //   Map :: (b -> c) -> Term b -> Term c
    //   LeafCons :: String -> Parser () -> Term c -> Term c
    //   RigidCons :: Parser () -> Term c -> Term c
    //   MalleableCons :: Parser b -> Term c -> Term (b, c)
    // 
    // parseAs :: String -> Parser a -> Term b -> Term (a, b)
    // parseAs _   _      Nil                 = error "key not found"
    // parseAs key parser (Map f t)           = Map (\(x, y) -> (x, f y))
    //                                        $ parseAs key parser t
    // parseAs key parser (LeafCons s p t)
    //                            | s == key  = MalleableCons parser t
    //                            | otherwise = LeafCons s p (parseAs key parser t)
    // parseAs key parser (RigidCons p t)     = RigidCons s p (parseAs key parser t)
    // parseAs key parser (MalleableCons p t) = Map (\(y, (x, z)) -> (x, (y, z)))
    //                                        $ MalleableCons p (parseAs key parser t)

    private[parser] sealed abstract trait Term[A] {
      type Parser[T] = XmlParsers.Parser[T]
      
      def parseAs[T](key: String, parser: Parser[T]): Term[(T,A)]
      
      def parser: Parser[A]
      
      // convenience method for the common combination term.parser.map
      def map[B](f: A => B): Parser[B] =
        parser.map(f)
    }

    private[parser] case class NilTerm() extends Term[Unit] {
      def parseAs[T](key: String, parser: Parser[T]): Term[(T,Unit)] =
        throw new java.lang.RuntimeException(s"key ${key} not found")
      
      def parser: Parser[Unit] =
        XmlParsers.success(())
    }

    private[parser] case class Map[A,B](
      inner: Term[A]
    )(
      f: A => B
    ) extends Term[B] {
      def parseAs[T](key: String, parser: Parser[T]): Term[(T,B)] =
        Map(inner.parseAs(key, parser)) {
          case (t, b) => (t, f(b))
        }
      
      def parser: Parser[B] =
        inner.parser.map(f)
    }

    // A rigid-until-further-notice leaf parser, such as <leaf/>.
    private[parser] case class LeafCons[A](
      headKey: String,
      headParser: XmlParsers.Parser[Unit],
      tail: Term[A]
    ) extends Term[A] {
      def parseAs[T](key: String, parser: Parser[T]): Term[(T,A)] =
        if (headKey == key) MalleableCons(parser, tail)
        else LeafCons(headKey, headParser, tail.parseAs(key, parser))
      
      def parser: Parser[A] =
        headParser ~> tail.parser
    }

    // A definitely-rigid parser, such as <foo value="bar"/>.
    private[parser] case class RigidCons[A](
      headParser: XmlParsers.Parser[Unit],
      tail: Term[A]
    ) extends Term[A] {
      def parseAs[T](key: String, parser: Parser[T]): Term[(T,A)] =
        RigidCons(headParser, tail.parseAs(key, parser))
      
      def parser: Parser[A] =
        headParser ~> tail.parser
    }

    // A leaf which has been replaced by a malleable parser, such as <int/> after
    // a parseAs("int", intParser).
    private[parser] case class MalleableCons[A,B](
      headParser: XmlParsers.Parser[A],
      tail: Term[B]
    ) extends Term[(A,B)] {
      def parseAs[T](key: String, parser: Parser[T]): Term[(T,(A,B))] =
        Map(MalleableCons(headParser, tail.parseAs(key, parser))) {
          case (a,(t,b)) => (t,(a,b))
        }
      
      def parser: Parser[(A,B)] =
        (headParser ~ tail.parser) map {
          case XmlParsers.~(a, b) => (a, b)
        }
    }

    def apply(nodeSeq: NodeSeq): Term[Unit] =
      {
        def go(tokens: List[XmlToken]): Term[Unit] =
          tokens match {
            case Nil => NilTerm()
            case XmlOpen(elem)
              :: XmlClose(elem2)
              :: tail
              if (elem eq elem2) && elem.attributes.isEmpty => {
                val key: String = elem.label
                val parser: XmlParsers.Parser[Unit] =
                  XmlParsers.xmlElem(elem).map(_ => ())
                LeafCons(key, parser, go(tail))
              }
            case XmlOpen(elem) :: tail =>
              {
                val parser: XmlParsers.Parser[Unit] =
                  XmlParsers.open(elem).map(_ => ())
                RigidCons(parser, go(tail))
              }
            case XmlClose(elem) :: tail =>
              {
                val parser: XmlParsers.Parser[Unit] =
                  XmlParsers.accept(s"</${elem.label}>", {
                    case XmlClose(e) if e.label == elem.label => ()
                  })
                RigidCons(parser, go(tail))
              }
            case token :: tail =>
              {
                val parser: XmlParsers.Parser[Unit] =
                  XmlParsers.elem(token).map(_ => ())
                RigidCons(parser, go(tail))
              }
          }
        
        go(XmlTokenReader(nodeSeq).toList)
      }
  }
}

object XmlParsers extends XmlParsers
