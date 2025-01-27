package utility

import org.apache.commons.io.IOUtils
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.io.InputStream
import java.net.URI
import java.nio.charset.StandardCharsets
import scala.util.Using


trait TestSuite extends AnyFunSuite with org.scalatest.TestSuite with Matchers {
  def mkString(inputStream: InputStream): String = IOUtils.toString(inputStream, StandardCharsets.UTF_8)

  def resourceAsString(path: String): String = Using.resource(getClass.getResourceAsStream(path))(mkString)
}
