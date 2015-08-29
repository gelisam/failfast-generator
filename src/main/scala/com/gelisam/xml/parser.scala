package com.gelisam.xml

import scala.xml._

/**
 * Points in-between xml nodes in the middle of an XML file.
 * Includes the nodes which follow the pointer, the nodes which follow the parent node, etc.
 * 
 * {{{
 * >>> XmlTail(<foo><bar/><baz/></foo>)
 * XmlTail(List(<foo><bar/><baz/></foo>))
 * }}}
 */
case class XmlTail(nodeSeqs: List[NodeSeq]) {
  /**
   * The node immediately after the pointer, if any.
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
}

object XmlTail {
  def apply(nodeSeq: NodeSeq): XmlTail =
    XmlTail(nodeSeq :: List())
}
