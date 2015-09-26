package com.example

import java.nio._
import java.security._
import javax.crypto._
import javax.crypto.spec._

class NotCipherdataException extends Exception

object PBAES {
  val DEFAULT_SALT_LENGTH = 10
  val DEFAULT_ITERATIONS = 1000
  val MAX_ITERATIONS = 100000
  val DEFAULT_KEY_SIZE = 128

  val rng = new SecureRandom

  private def makeSalt(length: Int) = {
    val salt = new Array[Byte](length)
    rng.nextBytes(salt)
    salt
  }

  def encrypt(password: Array[Char], plaintext: Array[Byte],
    saltLength: Int = DEFAULT_SALT_LENGTH,
    iterations: Int = DEFAULT_ITERATIONS,
    keySize: Int = DEFAULT_KEY_SIZE): Array[Byte] = {

    val salt = makeSalt(saltLength)

    val keyFactory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1")
    val keySpec = new PBEKeySpec(password, salt, iterations, keySize)
    val keyBytes = keyFactory.generateSecret(keySpec).getEncoded()
    val key = new SecretKeySpec(keyBytes, "AES")

    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    cipher.init(Cipher.ENCRYPT_MODE, key)
    val ciphertext = cipher.doFinal(plaintext)
    val iv = cipher.getIV()
    assert(iv.length == 16)

    // Format:
    // saltLength: Int (4 byte)
    // salt: Array[Byte] (saltLength bytes)
    // PKBDF iterations: Int (4 byte)
    // Key Length (in bits): Int (4 byte)
    // IV: Array[Byte] (16 byte)
    // ciphertext: Array[Byte] (rest of buffer)

    val resultLength = 4 + salt.length + 4 + 4 + 16 + ciphertext.length
    ByteBuffer.allocate(resultLength)
      .putInt(salt.length)
      .put(salt)
      .putInt(iterations)
      .putInt(keySize)
      .put(iv)
      .put(ciphertext)
      .array()
  }

  def decrypt(password: Array[Char], cipherdata: Array[Byte]): Array[Byte] = {
    try {
      val buf = ByteBuffer.wrap(cipherdata)
      val saltLength = buf.getInt()
      if (saltLength > cipherdata.length) {
        throw new NotCipherdataException
      }
      val salt = new Array[Byte](saltLength)
      buf.get(salt)
      val iterations = buf.getInt()
      if (iterations > MAX_ITERATIONS) {
        throw new NotCipherdataException
      }
      val keySize = buf.getInt()
      val iv = new Array[Byte](16)
      buf.get(iv)
      val ciphertext = new Array[Byte](buf.remaining())
      buf.get(ciphertext)

      val keyFactory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1")
      val keySpec = new PBEKeySpec(password, salt, iterations, keySize)
      val keyBytes = keyFactory.generateSecret(keySpec).getEncoded()
      val key = new SecretKeySpec(keyBytes, "AES")

      val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
      cipher.init(Cipher.DECRYPT_MODE, key, new IvParameterSpec(iv))
      cipher.doFinal(ciphertext)
    } catch {
      case _: BufferUnderflowException => throw new NotCipherdataException
      case _: InvalidKeyException => throw new NotCipherdataException
      case _: IllegalBlockSizeException => throw new NotCipherdataException
    }
  }
}