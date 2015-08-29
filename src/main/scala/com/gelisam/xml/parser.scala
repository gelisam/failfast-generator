package com.gelisam.xml

import scala.xml._

/**
 * An xml Elem in the middle of an XML file.
 * Includes the nodes which follow it, the nodes which follow its parent node, etc.
 * 
 * {{{
 * >>> XmlTail(<foo><bar/><baz/></foo>)
 * XmlTail(<foo><bar/><baz/></foo>,List())
 * }}}
 */
case class XmlTail(elem: Elem, nextNodes: List[NodeSeq] = List())
