package objects

import java.io.BufferedInputStream
import java.net.{InetAddress, ServerSocket, Socket}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object HttpServer {
  def bind(address: String, port: Int): Future[HttpServer] = {
    Future {
      val sock: ServerSocket = new ServerSocket(port, 5, InetAddress.getByName(address))
      sock.setReuseAddress(true)
      new HttpServer(sock)
    }
  }
}

class HttpServer(val servsock: ServerSocket) {

  def listen(callback: HttpRequest => Unit): Future[Unit] = {
    Future {
      while (true) {
        val client = servsock.accept()
        println(s"Got client: $client")

        read(client, callback)
        println("End of connection")
      }
    }
  }

  private def read(sock: Socket, callback: HttpRequest => Unit): Unit = {
    val buf = new Array[Byte](2048)
    val reader = new BufferedInputStream(sock.getInputStream)

    var alive = true
    while (alive) {
      val len = reader.read(buf)

      alive = len >= 0
      if (alive) {
        val msg = new String(buf, 0, len)
        val request = HttpRequest.build(msg, sock)

        callback(request)
      }
    }
  }

  override def toString: String = {
    s"${classOf[HttpServer].getSimpleName}[${servsock.getInetAddress}:${servsock.getLocalPort}]"
  }
}
