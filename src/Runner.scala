import java.nio.file.{Files, NoSuchFileException, Paths}

import objects.{ContentType, HttpHeaders, HttpServer, HttpStatus}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

object Runner {
  def fail(e: Throwable): Unit =
    println(s"Failed: $e")

  def main(args: Array[String]): Unit = {
    val basepath: String = args match {
      case Array(p, _*) => p
      case _ =>
        println("Usage: runner <path>")
        System.exit(1)
        throw new Exception(/* typeck */)
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

          Try(Files.readAllBytes(path)) match {
            case Success(content) =>
              val ext = uripath.lastIndexOf(".") match {
                case ix if ix >= 0 && uripath.length() > ix => uripath.substring(ix+1)
                case _ => ""
              }
              response.headers.set(HttpHeaders.contentTypeHeader, ContentType.mimeType(ext))
              response.write(content)
              response.send() andThen {
                case Success(_) => println("Sent page! \uD83D\uDE80")
                case Failure(e) => fail(e)
              }
            case Failure(ex: NoSuchFileException) =>
              println("NoSuchFile")
              response.statusCode = HttpStatus.notFound
              response.send() andThen {
                case Success(_) => println(s"Sent the 404 police \uD83D\uDE12 ($ex)")
                case Failure(e) => fail(e)
              }
            case Failure(ex) =>
              println("$ex")
              response.statusCode = HttpStatus.internalServerError
              response.send() andThen {
                case Success(_) => println(s"Server failure \uD83D\uDE12 ($ex)")
                case Failure(e) => fail(e)
              }
          }
        }
      },
      socket => {
        println(s"Got client: $socket")
      },
      _ => {
        println("End of connection")
      })
  }
}
