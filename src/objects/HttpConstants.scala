package objects

class StateException(s: String = null) extends Exception(s) {}

object HttpConstants {
  val HTTP11: Array[Byte] = "HTTP/1.1".getBytes
  val HTTP10: Array[Byte] = "HTTP/1.0".getBytes

  val COLON: Byte = 0x003A.toByte
  val SP: Byte = 0x0020.toByte
  val CR: Byte = 0x000D.toByte
  val LF: Byte = 0x000A.toByte
}

object HttpStatus {

  // Informational
  val continue = 100
  val switchingProtocols = 101

  // Success
  val ok = 200
  val created = 201
  val accepted = 202
  val nonAuthoritativeInformation = 203
  val noContent = 204
  val resetContent = 205
  val partialContent = 206

  // Redirection
  val multipleChoices = 300
  val movedPermanently = 301
  val found = 302
  val seeOther = 303
  val notModified = 304
  val useProxy = 305
  val temporaryRedirect = 307

  // Client error
  val badRequest = 400
  val unauthorized = 401
  val paymentRequired = 402
  val forbidden = 403
  val notFound = 404
  val methodNotAllowed = 405
  val notAcceptable = 406
  val proxyAuthenticationRequired = 407
  val requestTimeout = 408
  val conflict = 409
  val gone = 410
  val lengthRequired = 411
  val preconditionFailed = 412
  val requestEntityTooLarge = 413
  val requestUriTooLarge = 414
  val unsupportedMediaType = 415
  val requestedRangeNotSatisfiable = 416
  val expectationFailed = 417

  // Server error
  val internalServerError = 500
  val notImplemented = 501
  val badGateway = 502
  val serviceUnavailable = 503
  val gatewayTimeout = 504
  val httpVersionNotSupported = 505

  def reasonPhrase(statusCode: Int): String =
    statusCode match {
      case HttpStatus.continue                     => "Continue"
      case HttpStatus.switchingProtocols           => "Switching Protocols"
      case HttpStatus.ok                           => "OK"
      case HttpStatus.created                      => "Created"
      case HttpStatus.accepted                     => "Accepted"
      case HttpStatus.nonAuthoritativeInformation  => "Non-Authoritative Information"
      case HttpStatus.noContent                    => "No Content"
      case HttpStatus.resetContent                 => "Reset Content"
      case HttpStatus.partialContent               => "Partial Content"
      case HttpStatus.multipleChoices              => "Multiple Choices"
      case HttpStatus.movedPermanently             => "Moved Permanently"
      case HttpStatus.found                        => "Found"
      case HttpStatus.seeOther                     => "See Other"
      case HttpStatus.notModified                  => "Not Modified"
      case HttpStatus.useProxy                     => "Use Proxy"
      case HttpStatus.temporaryRedirect            => "Temporary Redirect"
      case HttpStatus.badRequest                   => "Bad Request"
      case HttpStatus.unauthorized                 => "Unauthorized"
      case HttpStatus.paymentRequired              => "Payment Required"
      case HttpStatus.forbidden                    => "Forbidden"
      case HttpStatus.notFound                     => "Not Found"
      case HttpStatus.methodNotAllowed             => "Method Not Allowed"
      case HttpStatus.notAcceptable                => "Not Acceptable"
      case HttpStatus.proxyAuthenticationRequired  => "Proxy Authentication Required"
      case HttpStatus.requestTimeout               => "Request Time-out"
      case HttpStatus.conflict                     => "Conflict"
      case HttpStatus.gone                         => "Gone"
      case HttpStatus.lengthRequired               => "Length Required"
      case HttpStatus.preconditionFailed           => "Precondition Failed"
      case HttpStatus.requestEntityTooLarge        => "Request Entity Too Large"
      case HttpStatus.requestUriTooLarge           => "Request-URI Too Large"
      case HttpStatus.unsupportedMediaType         => "Unsupported Media Type"
      case HttpStatus.requestedRangeNotSatisfiable => "Requested range not satisfiable"
      case HttpStatus.expectationFailed            => "Expectation Failed"
      case HttpStatus.internalServerError          => "Internal Server Error"
      case HttpStatus.notImplemented               => "Not Implemented"
      case HttpStatus.badGateway                   => "Bad Gateway"
      case HttpStatus.serviceUnavailable           => "Service Unavailable"
      case HttpStatus.gatewayTimeout               => "Gateway Time-out"
      case HttpStatus.httpVersionNotSupported      => "HTTP Version not supported"
      case _                                       =>s"Status $statusCode"
    }
}