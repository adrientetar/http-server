package objects

import java.net.{Socket, URI}
import java.util.{NoSuchElementException, StringTokenizer}

object HttpRequest {
  def build(msg: String, sock: Socket): HttpRequest = {
    val tok = new StringTokenizer(msg)
    /*
     * Parse header <method> <uri> HTTP/<protocolVersion>
     * ex: GET / HTTP/1.1
     */
    val method = tok.nextToken(" ")
    val uri = tok.nextToken(" ")
    val protocolVersion = tok.nextToken(" \r\n").replace("HTTP/", "")
    /*
     * Parse header fields <name>: <value>
     * ex: Connection: keep-alive
     */
    val headers = new HttpHeaders(protocolVersion)
    try {
      while (true) {
        val header = tok.nextToken("\r\n").split(": ", 2)
        assert(header.length == 2)

        headers.set(header(0), header(1))
      }
    }
    catch {
      case _: NoSuchElementException =>
      case _: AssertionError =>
    }

    val uri_ = new URI(uri)
    val response = new HttpResponse(uri_, protocolVersion, sock)

    new HttpRequest(response, headers, method, uri_)
  }
}

class HttpRequest(val response: HttpResponse,
                  val headers: HttpHeaders, val method: String, val uri: URI) {

  def protocolVersion: String = headers.protocolVersion

  override def toString: String = {
    s"${classOf[HttpRequest].getSimpleName}[method=$method,uri=$uri,protocolVersion=$protocolVersion]"
  }
}
