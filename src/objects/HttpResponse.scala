package objects

import java.io.{BufferedOutputStream, ByteArrayOutputStream, OutputStream}
import java.net.{Socket, URI}
import java.nio.charset.StandardCharsets

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class HttpResponse(val uri: URI, val protocolVersion: String, val sock: Socket) {
  private val _out: ByteArrayOutputStream = new ByteArrayOutputStream()
  private var _frozen: Boolean = false
  private var _statusCode: Int = HttpStatus.ok

  val headers: HttpHeaders = HttpHeaders.defaultResponseHeaders(protocolVersion)

  def reasonPhrase: String = HttpStatus.reasonPhrase(statusCode)

  def statusCode: Int = _statusCode
  def statusCode_= (value: Int): Unit = {
    _checkMutable()

    _statusCode = value
  }

  def send(): Future[Unit] = {
    _checkMutable(/* freeze before spawning thread */ true)

    Future {
      val output = new BufferedOutputStream(sock.getOutputStream)
      val msg = _out.toByteArray

      // Set content length
      headers.contentLength = msg.length
      headers._finalize()

      // Write header
      _writeHeader(output)

      // Write body
      output.write(msg)

      output.flush()
    }
  }

  def write(content: Array[Byte]): Unit = {
    _checkMutable()

    _out.write(content)
  }
  def write(content: String): Unit = {
    write(content.getBytes(StandardCharsets.UTF_8))
  }

  private def _checkMutable(freeze: Boolean = false): Unit = {
    if (_frozen) {
      throw new StateException("Response is frozen at this time")
    }
    _frozen |= freeze
  }

  private def _writeHeader(output: OutputStream): Unit = {
    // Write status line
    if (headers.protocolVersion == "1.1") {
      output.write(HttpConstants.HTTP11)
    } else {
      output.write(HttpConstants.HTTP10)
    }
    output.write(HttpConstants.SP)
    output.write(statusCode.toString.getBytes)
    output.write(HttpConstants.SP)
    output.write(reasonPhrase.getBytes)
    output.write(HttpConstants.CR)
    output.write(HttpConstants.LF)

    headers.write(output)
    output.write(HttpConstants.CR)
    output.write(HttpConstants.LF)
  }

  override def toString: String = {
    s"${classOf[HttpResponse].getSimpleName}[uri=$uri,statusCode=$statusCode,protocolVersion=$protocolVersion]"
  }
}
