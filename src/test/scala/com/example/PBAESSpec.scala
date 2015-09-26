package com.example

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import sun.misc.BASE64Encoder
import java.nio.charset.StandardCharsets

@RunWith(classOf[JUnitRunner])
class PBAESSpec extends Specification {
  "PBAES" should {
    "pass roundtrip with simple string" in {
      val password = "testpass".toCharArray()
      val plaintextString = "this is some random text"
      val plaintext = plaintextString.getBytes(StandardCharsets.UTF_8)

      val ciphertext = PBAES.encrypt(password, plaintext)
      System.out.println(new BASE64Encoder().encode(ciphertext))
      new String(PBAES.decrypt(password, ciphertext), StandardCharsets.UTF_8) === plaintextString
    }
  }
}