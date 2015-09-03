package com.gelisam.parser

import scala.util.parsing.input._

/**
 * Have your Reader extend this to gain a more useful toString, as well as the
 * ability to map and filter.
 * 
 * {{{
 * >>> import com.gelisam.parser.xml._
 * 
 * // toString lists the remaining elements
 * >>> XmlTokenReader(<parent><child/></parent>)
 * XmlTokenReader(XmlOpen(<parent><child/></parent>),XmlOpen(<child/>),XmlClose(<child/>),XmlClose(<parent><child/></parent>))
 * 
 * >>> XmlTokenReader(<parent><child/></parent>) map {
 * ...   case XmlOpen(_) => XmlOpen(<open/>)
 * ...   case XmlClose(_) => XmlClose(<close/>)
 * ...   case token => token
 * ... }
 * XmlTokenReader(XmlOpen(<open/>),XmlOpen(<open/>),XmlClose(<close/>),XmlClose(<close/>))
 * }}}
 * 
 * >>> XmlTokenReader(<parent><child/></parent>) filter {
 * ...   case XmlOpen(_) => true
 * ...   case _ => false
 * ... }
 * XmlTokenReader(XmlOpen(<parent><child/></parent>),XmlOpen(<child/>))
 * }}}
 */
trait ReaderUtil[A] extends Reader[A] {
  override def rest: ReaderUtil[A]
  
  def toList: List[A] =
    if (atEnd) Nil
    else first :: rest.toList
  
  def className: String =
    getClass.getName.split("\\.").last
  
  // simplify debugging by listing all the remaining elements
  override def toString: String =
    {
      val elems = toList mkString ","
      s"${className}(${elems})"
    }
  
  def map[B](f: A => B): ReaderUtil[B] =
    new MappedReader(this, f)
  
  def filter[B](p: A => Boolean): ReaderUtil[A] =
    new FilteredReader(this, p)
}

class MappedReader[A,B](inner: ReaderUtil[A], f: A => B) extends ReaderUtil[B] {
  def atEnd: Boolean = inner.atEnd
  def first: B = f(inner.first)
  def pos: Position = inner.pos
  def rest: ReaderUtil[B] = new MappedReader(inner.rest, f)
  
  override def className: String =
    inner.className
}

class FilteredReader[A](var inner: ReaderUtil[A], p: A => Boolean) extends ReaderUtil[A] {
  while (!inner.atEnd && !p(inner.first)) {
    inner = inner.rest
  }
  
  def atEnd: Boolean = inner.atEnd
  def first: A = inner.first
  def pos: Position = inner.pos
  def rest: ReaderUtil[A] = new FilteredReader(inner.rest, p)
  
  override def className: String =
    inner.className
}
