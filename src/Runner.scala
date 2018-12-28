import java.nio.file.{Files, NoSuchFileException, Paths}

import objects._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

object Runner {
  def main(args: Array[String]): Unit = {
    val basepath: String = args match {
      case Array(p, _*) => p
      case _ =>
        println("Usage: runner <path>")
        System.exit(1)
        /* typeck */ throw new Exception()
    }

    val address = "127.0.0.1"
    val port = 8080
    print("Magnéto Serge... ")
    val server = HttpServer.bind(address, port)
    println("✔")

    println(s"$address:$port... 👀")
    server.listen(request => {
        println(s"Ack: $request")

        if (request.method == "GET") {
          val response = request.response
          val uripath = request.uri.toString match {
            case "/" => "/index.html"
            case p => p
          }
          val path = Paths.get(basepath, uripath)

          val msg = Try(Files.readAllBytes(path)) match {
            case Success(content) =>
              val ext = uripath.lastIndexOf(".") match {
                case ix if ix >= 0 && uripath.length() > ix => uripath.substring(ix+1)
                case _ => ""
              }
              response.headers.set(HttpHeaders.contentTypeHeader, ContentType.mimeType(ext))
              response.write(content)

              "Sent page! \uD83D\uDE80"
            case Failure(_: NoSuchFileException) =>
              response.statusCode = HttpStatus.notFound

              "Sent the 404 police \uD83D\uDE12"
            case Failure(ex) =>
              response.statusCode = HttpStatus.internalServerError

              s"Server failure \uD83D\uDE12 ($ex)"
          }

          send_or(response, msg)
        }
      },
      socket => {
        println(s"Got client: $socket")
      },
      _ => {
        println("End of connection")
      })
  }

  def send_or(response: HttpResponse, msg: String): Unit = {
    response.send() andThen {
      case Success(_) => println(msg)
      case Failure(e) => println(s"Failed: $e")
    }
  }
}
