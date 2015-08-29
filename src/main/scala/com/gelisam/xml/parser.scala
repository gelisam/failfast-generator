package com.gelisam.xml.parser

import scala.xml._

/**
 * Points in-between xml nodes in the middle of an XML file.
 * Includes the nodes which follow the pointer, the nodes which follow the parent node, etc.
 * 
 * {{{
 * >>> XmlTail(<foo><bar/><baz/></foo>)
 * <POINTER/><foo><bar/><baz/></foo>
 * }}}
 */
class XmlTail(nodeSeqs: List[NodeSeq]) {
  override def toString: String =
    nodeSeqs match {
      case Nil =>
        <POINTER/>.toString
      case nodeSeq :: nodeSeqs =>
        nodeSeqs.foldLeft[NodeSeq](<POINTER/> ++ nodeSeq)((xmlWithPointer, nodeSeq) =>
          <parent>{xmlWithPointer}</parent> ++ nodeSeq
        ).toString
    }
  
  def isEmpty: Boolean =
    nodeSeqs.isEmpty
  
  /**
   * The next node, if any.
   * None if pointing after the last child of a parent node.
   * 
   * {{{
   * >>> XmlTail(<foo><bar/><baz/></foo>).headOption
   * Some(<foo><bar/><baz/></foo>)
   * }}}
   */
  def headOption: Option[Node] =
    for {
      nodeSeq <- nodeSeqs.headOption
      node <- nodeSeq.headOption
    } yield node
  
  /**
   * The next node, if any, and the tail.
   * None if pointing after the last child of a parent node.
   * 
   * {{{
   * >>> XmlTail(<foo><bar/><baz/></foo>).unconsOption
   * Some((<foo><bar/><baz/></foo>,<POINTER/>))
   * }}}
   */
  def unconsOption: Option[(Node, XmlTail)] =
    for {
      nodeSeq <- nodeSeqs.headOption
      node <- nodeSeq.headOption
    } yield (node, new XmlTail(nodeSeq.tail :: nodeSeqs.tail))
  
  /**
   * Point before the first child of the next node, even if it doesn't have any children.
   * None if pointing after the last child of a parent node.
   * 
   * {{{
   * // inside foo
   * >>> XmlTail(<foo><bar/><baz/></foo>).downOption
   * Some(<parent><POINTER/><bar/><baz/></parent>)
   * 
   * // inside bar
   * >>> XmlTail(<foo><bar/><baz/></foo>).downOption.flatMap(_.downOption)
   * Some(<parent><parent><POINTER/></parent><baz/></parent>)
   * 
   * // nothing in which to go inside
   * >>> XmlTail(<foo><bar/><baz/></foo>).downOption.flatMap(_.downOption).flatMap(_.downOption)
   * None
   * }}}
   */
  def downOption: Option[XmlTail] =
    for {
      nodeSeq <- nodeSeqs.headOption
      node <- nodeSeq.headOption
      elem <- node match {
        case elem: Elem => Some(elem)
        case _ => None
      }
    } yield new XmlTail(NodeSeq.fromSeq(elem.child) :: nodeSeq.tail :: nodeSeqs.tail)
  
  /**
   * Point after the parent node.
   * None unless pointing after the last child of a parent node.
   * 
   * {{{
   * // after bar
   * >>> XmlTail(<foo><bar/><baz/></foo>).downOption.flatMap(_.downOption).flatMap(_.upOption)
   * Some(<parent><POINTER/><baz/></parent>)
   * 
   * // nothing in which to go outside
   * >>> XmlTail(<foo><bar/><baz/></foo>).downOption.flatMap(_.upOption)
   * None
   * 
   * // nothing in which to go outside
   * >>> XmlTail(<foo><bar/><baz/></foo>).upOption
   * None
   * }}}
   */
  def upOption: Option[XmlTail] =
    nodeSeqs match {
      case nodeSeq :: nodeSeqs if nodeSeq.isEmpty => Some(new XmlTail(nodeSeqs))
      case _ => None
    }
}

object XmlTail {
  def apply(nodeSeq: NodeSeq): XmlTail =
    new XmlTail(nodeSeq :: List())
}


class XmlParser[A](runXmlParser: XmlTail => Option[(A, XmlTail)]) {
  def parsePartially(nodeSeq: NodeSeq): Option[(A, XmlTail)] =
    runXmlParser(XmlTail(nodeSeq))
  
  def parseAll(nodeSeq: NodeSeq): Option[A] =
    parsePartially(nodeSeq).flatMap {
      case (a, t) if t.isEmpty => Some(a)
      case _ => None
    }
  
  /**
   * {{{
   * >>> XmlParser.node.map(_.text.toInt).parsePartially(<number>42</number>)
   * Some((42,<POINTER/>))
   * }}}
   */
  def map[B](f: A => B): XmlParser[B] =
    new XmlParser(t =>
      runXmlParser(t) map {
        case (a, t) => (f(a), t)
      }
    )
}

object XmlParser {
  def success[A](a: A): XmlParser[A] =
    new XmlParser(t => Some(a, t))
  
  def failure(): XmlParser[Nothing] =
    new XmlParser(_ => None)
  
  def node(): XmlParser[Node] =
    new XmlParser(_.unconsOption)
  
  /**
   * Consume the next node.
   * Fail if the node is not the one we expect.
   * 
   * {{{
   * >>> XmlParser.elem(<foo/>).parsePartially(<foo/><bar/>)
   * Some((<foo/>,<POINTER/><bar/>))
   * 
   * >>> XmlParser.elem(<bar/>).parsePartially(<foo/><bar/>)
   * None
   * }}}
   */
  def elem(expected: Node): XmlParser[Node] =
    new XmlParser(t => t.unconsOption filter {
      case (node,_) => node == expected
    })
}
