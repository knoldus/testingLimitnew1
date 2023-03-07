package com.knoldus.leader_board

import akka.http.scaladsl.model._
import java.security.MessageDigest
import akka.http.scaladsl.server.directives.FileInfo
import java.io.File
import java.util.UUID

object Base64 {

  final case class B64Scheme(encodeTable: Array[Char], strictPadding: Boolean = true,
                       postEncode: String => String = identity,
                       preDecode: String => String = identity)

  val base64 =  B64Scheme((('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') ++ Seq('+', '/')).toArray)

  implicit class SeqEncoder(s: Seq[Byte]) {
    def toBase64(implicit scheme: B64Scheme = base64): String = Encoder(s.toArray).toBase64
  }

  implicit class Encoder(b: Array[Byte]) {
    private[this] val r = new java.lang.StringBuilder((b.length + 3) * 4 / 3)
    lazy val pad = (3 - b.length % 3) % 3

    def toBase64(implicit scheme: B64Scheme = base64): String = {
      def sixBits(x: Byte, y: Byte, z: Byte): Unit = {
        val zz = (x & 0xff) << 16 | (y & 0xff) << 8 | (z & 0xff)
        r append scheme.encodeTable(zz >> 18)
        r append scheme.encodeTable(zz >> 12 & 0x3f)
        r append scheme.encodeTable(zz >> 6 & 0x3f)
        r append scheme.encodeTable(zz & 0x3f)
      }
      for (p <- 0 until b.length - 2 by 3) {
        sixBits(b(p), b(p + 1), b(p + 2))
      }
      pad match {
        case 0 =>
        case 1 => sixBits(b(b.length - 2), b(b.length - 1), 0)
        case 2 => sixBits(b(b.length - 1), 0, 0)
      }
      r setLength (r.length - pad)
      r append "=" * pad
      scheme.postEncode("data:image/jpeg;base64," + r.toString)
    }
  }

  def tempDestination(fileInfo: FileInfo): File = {
    File.createTempFile(UUID.randomUUID().toString, generateFileName(fileInfo.getContentType))
  }

  def generateFileName(contentType: ContentType): String = {
    val currentTimeMillis = System.currentTimeMillis()
    val extension = contentType.mediaType match {
      case MediaTypes.`image/png` => ".jpeg"
      case MediaTypes.`image/jpeg` => ".jpeg"
      case MediaTypes.`image/webp` => ".jpeg"
      case MediaTypes.`image/tiff` => ".jpeg"
      case _ => ".tmp"

    }
    md5Hash("Image Title" + "-" + currentTimeMillis) + extension
  }

  def md5Hash(input: String): String = {
    val digest: MessageDigest = MessageDigest.getInstance("MD5")
    digest.reset()

    def encode(b: Byte): String = java.lang.Integer.toString(b & 0xff, 36)

    ("" /: digest.digest(input.getBytes("UTF-8")))(_ + encode(_))
  }
}
