import objects.{ContentType, HttpServer, HttpStatus}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object Runner {
  def fail(e: Throwable): Unit =
    println(s"Failed: $e")

  def main(args: Array[String]): Unit = {
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

          request.uri.toString match {
            case "/" =>
              response.headers.contentType = ContentType.html
              response.write("<div style=\"font-family: 'Source Code Pro', monospace; font-size: 36\">\n")
              response.write("Scala says <span style=\"color: #df3b3a\">hi</span>.\n")
              response.write("</div>")
              response.send() andThen {
                case Success(_) => println("Sent my page! \uD83D\uDE80")
                case Failure(e) => fail(e)
              }
            case _ =>
              response.statusCode = HttpStatus.notFound
              response.send() andThen {
                case Success(_) => println("Sent the 404 police \uD83D\uDE12")
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
