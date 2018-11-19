package objects

import java.io.BufferedInputStream
import java.net.{InetAddress, ServerSocket, Socket}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object HttpServer {
  def bind(address: String, port: Int): HttpServer = {
    val sock: ServerSocket = new ServerSocket(port, 5, InetAddress.getByName(address))
    sock.setReuseAddress(true)
    new HttpServer(sock)
  }
}

class HttpServer(val servsock: ServerSocket) {

  def listen(onRequest: HttpRequest => Unit,
             onConnect: Socket => Unit = _ => (), onClose: Socket => Unit = _ => ()): Unit = {
    while (true) {
      val client = servsock.accept()
      onConnect(client)

      val response = _handleRequest(client, onRequest)

      response.onComplete(_ => {
        onClose(client)
      })
    }
  }

  private def _handleRequest(sock: Socket, callback: HttpRequest => Unit): Future[Unit] = {
    Future {
      val buffer = new Array[Byte](2048)
      val reader = new BufferedInputStream(sock.getInputStream)

      var len = 0
      while ({
        len = reader.read(buffer); len > 0
      }) {
        val msg = new String(buffer, 0, len)
        val request = HttpRequest.build(msg, sock)

        callback(request)
      }
    }
  }

  override def toString: String = {
    s"${classOf[HttpServer].getSimpleName}[${servsock.getInetAddress}:${servsock.getLocalPort}]"
  }
}
