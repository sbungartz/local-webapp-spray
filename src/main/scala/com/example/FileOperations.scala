package com.example

import spray.http._
import spray.routing._
import StatusCodes._
import Directives._
import java.io._
import java.nio._
import java.nio.file._
import java.nio.charset.StandardCharsets
import java.security._
import javax.crypto._
import javax.crypto.spec._
import akka.actor.ActorRefFactory
import spray.routing.directives.ContentTypeResolver

object FileOperations {
  def getFromFileEncrypted(file: File, password: Array[Char])(implicit settings: RoutingSettings, resolver: ContentTypeResolver, refFactory: ActorRefFactory): Route = {
    get {
      detach() {
        complete {
          try {
            val encrypted = Files.readAllBytes(file.toPath())
            PBAES.decrypt(password, encrypted)
          } catch {
            case _: NoSuchFileException => NotFound
            case _: AccessDeniedException => Forbidden
            case _: NotCipherdataException => BadRequest
          }
        }
      }
    }
  }

  def putToFileEncrypted(file: File, password: Array[Char])(implicit settings: RoutingSettings, resolver: ContentTypeResolver, refFactory: ActorRefFactory): Route = {
    put {
      entity(as[Array[Byte]]) { data =>
        detach() {
          complete {
            try {
              val encrypted = PBAES.encrypt(password, data)
              Files.write(file.toPath(), encrypted)
              "ok"
            } catch {
              case _: NoSuchFileException => NotFound
              case _: AccessDeniedException => Forbidden
            }
          }
        }
      }
    }
  }

  def getFromFilePlain(file: File)(implicit settings: RoutingSettings, resolver: ContentTypeResolver, refFactory: ActorRefFactory): Route = {
    get {
      detach() {
        complete {
          try {
            Files.readAllBytes(file.toPath())
          } catch {
            case _: NoSuchFileException => NotFound
            case _: AccessDeniedException => Forbidden
          }
        }
      }
    }
  }

  def putToFilePlain(file: File)(implicit settings: RoutingSettings, resolver: ContentTypeResolver, refFactory: ActorRefFactory): Route = {
    put {
      entity(as[Array[Byte]]) { data =>
        detach() {
          complete {
            try {
              Files.write(file.toPath(), data)
              "ok"
            } catch {
              case _: NoSuchFileException => NotFound
              case _: AccessDeniedException => Forbidden
            }
          }
        }
      }
    }
  }
}