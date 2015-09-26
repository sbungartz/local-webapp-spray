package com.example

import akka.event.Logging
import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest
import spray.http._
import StatusCodes._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermissions
import java.nio.ByteBuffer

@RunWith(classOf[JUnitRunner])
class RoutingServiceSpec extends Specification with Specs2RouteTest with RoutingService {
  def log = Logging(system, classOf[RoutingServiceSpec].getName)
  def actorRefFactory = system
  override def homeDir = new java.io.File("var-test")

  "RoutingService" should {
    "return a greeting for GET requests to the root path" in {
      Get() ~> myRoute ~> check {
        responseAs[String] must contain("Wassup dawg")
      }
    }

    "leave GET requests to other paths unhandled" in {
      Get("/kermit") ~> myRoute ~> check {
        handled must beFalse
      }
    }

    "return a MethodNotAllowed error for PUT requests to the root path" in {
      Put() ~> sealRoute(myRoute) ~> check {
        status === MethodNotAllowed
        responseAs[String] === "HTTP method not allowed, supported methods: GET"
      }
    }
  }

  "Storage API" should {
    "store and load non-encrpyted file" in {
      Put("/api/storage/non-enctest", "test content") ~> myRoute ~> check {
        status === OK
        responseAs[String] === "ok"
      }

      Files.readAllBytes(Paths.get("var-test/non-enctest")) must beEqualTo("test content".getBytes())

      Get("/api/storage/non-enctest") ~> myRoute ~> check {
        status === OK
        responseAs[String] === "test content"
      }
    }

    "store and load encrypted file" in {
      Put("/api/storage/enctest?pw=testpw", "test content") ~> myRoute ~> check {
        status === OK
        responseAs[String] === "ok"
      }

      // Since for the API user encryption happens transperently, the results for
      // the API requests stay the same.
      // The only difference to the previous test is, that now, here, the file content
      // must be different from the plain text.
      Files.readAllBytes(Paths.get("var-test/enctest")) must not(beEqualTo("test content".getBytes()))

      Get("/api/storage/enctest?pw=testpw") ~> myRoute ~> check {
        status === OK
        responseAs[String] === "test content"
      }
    }

    "respond NotFound for GET on missing file" in {
      Get("/api/storage/missingfile") ~> sealRoute(myRoute) ~> check {
        status === NotFound
      }

      Get("/api/storage/missingfile?pw=testpass") ~> sealRoute(myRoute) ~> check {
        status === NotFound
      }
    }

    "respond Forbidden for GET and PUT on file without read/write access" in {
      try {
        Files.createFile(Paths.get("var-test/forbidden-file"))
        Files.setPosixFilePermissions(Paths.get("var-test/forbidden-file"), PosixFilePermissions.fromString("---------"))

        Get("/api/storage/forbidden-file") ~> sealRoute(myRoute) ~> check {
          status === Forbidden
        }

        Get("/api/storage/forbidden-file?pw=testpw") ~> sealRoute(myRoute) ~> check {
          status === Forbidden
        }

        Put("/api/storage/forbidden-file", "test content") ~> sealRoute(myRoute) ~> check {
          status === Forbidden
        }

        Put("/api/storage/forbidden-file?pw=testpw", "test content") ~> sealRoute(myRoute) ~> check {
          status === Forbidden
        }
      } finally {
        Files.delete(Paths.get("var-test/forbidden-file"))
      }
    }

    "respond NotFound for PUT in non-existing directory" in {
      Put("/api/storage/missing-directory/file", "test content") ~> sealRoute(myRoute) ~> check {
        status === NotFound
      }

      Put("/api/storage/missing-directory/file?pw=testpw", "test content") ~> sealRoute(myRoute) ~> check {
        status === NotFound
      }
    }

    "respond Forbidden for GET and PUT in directory without read/write access" in {
      try {
        Files.createDirectory(Paths.get("var-test/forbidden-directory"))
        Files.setPosixFilePermissions(Paths.get("var-test/forbidden-directory"), PosixFilePermissions.fromString("---------"))

        Get("/api/storage/forbidden-directory/file") ~> sealRoute(myRoute) ~> check {
          status === Forbidden
        }

        Get("/api/storage/forbidden-directory/file?pw=testpw") ~> sealRoute(myRoute) ~> check {
          status === Forbidden
        }

        Put("/api/storage/forbidden-directory/file", "test content") ~> sealRoute(myRoute) ~> check {
          status === Forbidden
        }

        Put("/api/storage/forbidden-directory/file?pw=testpw", "test content") ~> sealRoute(myRoute) ~> check {
          status === Forbidden
        }
      } finally {
        Files.delete(Paths.get("var-test/forbidden-directory"))
      }
    }

    "respond with garbage when opening encrypted file with wrong password" in {
      Put("/api/storage/encfile-opened-with-wrong-pw?pw=testpw", "test content") ~> myRoute ~> check {
        status === OK
      }

      Get("/api/storage/encfile-opened-with-wrong-pw?pw=wrongpw") ~> myRoute ~> check {
        responseAs[String] !== "test content"
      }
    }

    "respond with BadRequest when opening non-encrypted file with password" in {
      Put("/api/storage/plain-file-opened-with-pw", "test content") ~> myRoute ~> check {
        status === OK
      }

      Get("/api/storage/plain-file-opened-with-pw?pw=testpw") ~> sealRoute(myRoute) ~> check {
        status === BadRequest
      }

      // very high number of iterations:
      Put("/api/storage/plain-file-opened-with-pw", ByteBuffer.allocate(100).putInt(4).putInt(32).putInt(Int.MaxValue).putInt(128).array()) ~> myRoute ~> check {
        status === OK
      }

      Get("/api/storage/plain-file-opened-with-pw?pw=testpw") ~> sealRoute(myRoute) ~> check {
        status === BadRequest
      }

      // illegal key size
      Put("/api/storage/plain-file-opened-with-pw", ByteBuffer.allocate(100).putInt(4).putInt(32).putInt(1000).putInt(25).array()) ~> myRoute ~> check {
        status === OK
      }

      Get("/api/storage/plain-file-opened-with-pw?pw=testpw") ~> sealRoute(myRoute) ~> check {
        status === BadRequest
      }

      // file too short
      Put("/api/storage/plain-file-opened-with-pw", ByteBuffer.allocate(1).array()) ~> myRoute ~> check {
        status === OK
      }

      Get("/api/storage/plain-file-opened-with-pw?pw=testpw") ~> sealRoute(myRoute) ~> check {
        status === BadRequest
      }

      // file header matches, but then not long enough
      Put("/api/storage/plain-file-opened-with-pw", ByteBuffer.allocate(100).putInt(4).putInt(32).putInt(1000).putInt(128).array()) ~> myRoute ~> check {
        status === OK
      }

      Get("/api/storage/plain-file-opened-with-pw?pw=testpw") ~> sealRoute(myRoute) ~> check {
        status === BadRequest
      }
    }
  }
}
