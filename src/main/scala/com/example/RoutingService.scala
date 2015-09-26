package com.example

import akka.actor.Actor
import spray.routing._
import spray.http._
import MediaTypes._
import java.io._
import java.nio.file._

import java.security._
import javax.crypto._

// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
class RoutingServiceActor extends Actor with RoutingService with akka.actor.ActorLogging {

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing
  // or timeout handling
  def receive = runRoute(myRoute)
}

// this trait defines our service behavior independently from the service actor
trait RoutingService extends HttpService {
  def log: akka.event.LoggingAdapter
  def homeDir = new File(System.getProperty("user.home"))

  // format: OFF
  val myRoute =
    pathPrefix("api") {
      path("storage" / Rest) { pathRest =>
        val filePath = new File(homeDir, pathRest)

        parameterMap { params =>
          import FileOperations._
          
          params.get("pw") match {
            case Some(password) => {
              getFromFileEncrypted(filePath, password.toCharArray()) ~
              putToFileEncrypted(filePath, password.toCharArray())
            }
            case None => {
              getFromFilePlain(filePath) ~
              putToFilePlain(filePath)
            }
          }
        }
      }
    } ~
    path("") {
      getFromFile("webapp/index.html")
    } ~
    getFromDirectory("webapp")
   // format: ON
}
