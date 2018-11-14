package objects

import java.io.OutputStream

import objects.ContentType.ContentType

import scala.collection.mutable

object ContentType extends Enumeration {
  type ContentType = Value

  val binary = Value("application/octet-stream")
  val html = Value("text/html; charset=utf-8")
  val json = Value("application/json; charset=utf-8")
  val text = Value("text/plain; charset=utf-8")
}

object HttpHeaders {
  val acceptHeader = "Accept"
  val acceptEncodingHeader = "Accept-Encoding"
  val acceptLanguageHeader = "Accept-Language"
  val cacheControlHeader = "Cache-Control"
  val connectionHeader = "Connection"
  val contentLengthHeader = "Content-Length"
  val contentTypeHeader = "Content-Type"
  val hostHeader = "Host"
  val userAgentRequest = "User-Agent"

  def defaultResponseHeaders(protocolVersion: String): HttpHeaders = {
    val headers = new HttpHeaders(protocolVersion)
    headers.contentType = ContentType.text
    headers
  }
}

class HttpHeaders(val protocolVersion: String) {
  private var _fini = false
  private val _headers = new mutable.HashMap[String, String]()

  private[objects] def fini = _fini

  def contentLength: Int = {
    value(HttpHeaders.contentTypeHeader) match {
      case Some(value) => value.toInt
      case None => -1
    }
  }
  def contentLength_= (value: Int): Unit = {
    value match {
      case v if v >= 0 => set(HttpHeaders.contentLengthHeader, v)
      case _ => remove(HttpHeaders.contentLengthHeader)
    }
  }

  def contentType: Option[ContentType.Value] = {
    val s = value(HttpHeaders.contentTypeHeader)
    ContentType.values.find(_.toString == s)
  }
  def contentType_= (value: ContentType): Unit = set(HttpHeaders.contentTypeHeader, value.toString)

  def iterator(): Iterator[(String, String)] = {
    _headers.iterator
  }

  def remove(name: String): Unit = {
    _checkMutable()

    _headers.remove(name)
  }

  def set(name: String, value: Any): Unit = {
    _checkMutable()

    _headers(name) = value.toString
  }

  def value(name: String): Option[String] = {
    _headers.get(name)
  }

  def write(output: OutputStream): Unit = {
    for ((name, value) <- _headers.iterator) {
      output.write(name.getBytes)
      output.write(HttpConstants.COLON)
      output.write(HttpConstants.SP)
      output.write(value.getBytes)
      output.write(HttpConstants.CR)
      output.write(HttpConstants.LF)
    }
  }

  private def _checkMutable(): Unit = {
    if (_fini) {
      throw new StateException("Headers are frozen at this time")
    }
  }

  private[objects] def _finalize(): Unit = {
    _fini = true
  }

  override def toString: String = {
    _headers.map{ case (name, value) => s"$name: $value" }.mkString("\n")
  }
}