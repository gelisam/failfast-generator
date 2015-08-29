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
case class XmlTail(runXmlTail: List[NodeSeq])

object XmlTail {
  def apply(elem: Elem): XmlTail =
    XmlTail(elem.repr :: List())
}
