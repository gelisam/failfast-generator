package com.gelisam.parser

import scala.util.parsing.input._

/**
 * Have your Reader extend this to gain a more useful toString.
 * 
 * {{{
 * >>> import com.gelisam.parser.xml._
 * 
 * // toString lists the remaining elements
 * >>> XmlTokenReader(<parent><child/></parent>)
 * XmlTokenReader(XmlOpen(<parent><child/></parent>),XmlOpen(<child/>),XmlClose(<child/>),XmlClose(<parent><child/></parent>))
 * }}}
 */
trait ReaderUtil[A] extends Reader[A] {
  override def rest: ReaderUtil[A]
  
  def toList: List[A] =
    if (atEnd) Nil
    else first :: rest.toList
  
  // simplify debugging by listing all the remaining elements
  override def toString: String =
    {
      val qualifiedName: String = getClass.getName
      val shortName: String = qualifiedName.split("\\.").last
      val elems = toList mkString ","
      s"${shortName}(${elems})"
    }
}
