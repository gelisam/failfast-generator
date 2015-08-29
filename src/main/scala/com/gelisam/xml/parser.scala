package com.gelisam.xml

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
case class XmlTail(nodeSeqs: List[NodeSeq]) {
  override def toString: String =
    nodeSeqs match {
      case Nil =>
        <POINTER/>.toString
      case nodeSeq :: nodeSeqs =>
        nodeSeqs.foldLeft[NodeSeq](<POINTER/>.repr ++ nodeSeq)((xmlWithPointer, nodeSeq) =>
          <parent>{xmlWithPointer}</parent>.repr ++ nodeSeq
        ).toString
    }
  
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
    } yield XmlTail(NodeSeq.fromSeq(elem.child) :: nodeSeq.tail :: nodeSeqs.tail)
  
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
      case nodeSeq :: nodeSeqs if nodeSeq.isEmpty => Some(XmlTail(nodeSeqs))
      case _ => None
    }
}

object XmlTail {
  def apply(nodeSeq: NodeSeq): XmlTail =
    XmlTail(nodeSeq :: List())
}
