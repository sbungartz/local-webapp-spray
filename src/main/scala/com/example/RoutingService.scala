package com.example

import akka.actor.Actor
import spray.routing._
import spray.http._
import MediaTypes._
import java.io._

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

class Loan[A <: AutoCloseable](resource: A) {
  def to[B](block: A => B) = {
    var t: Throwable = null
    try {
      block(resource)
    } catch {
      case x: Throwable => t = x; throw x
    } finally {
      if (resource != null) {
        if (t != null) {
          try {
            resource.close()
          } catch {
            case y: Throwable => t.addSuppressed(y)
          }
        } else {
          resource.close()
        }
      }
    }
  }
}

object Loan {
  def loan[A <: AutoCloseable](resource: A) = new Loan(resource)
}

// this trait defines our service behavior independently from the service actor
trait RoutingService extends HttpService {
  def log: akka.event.LoggingAdapter
   
  val myRoute =
    pathPrefix("api") {
      path("storage" / Rest) { pathRest =>
        val homeDir = new File(System.getProperty("user.home"))
        val filePath = new File(homeDir, pathRest)

        getFromFile(filePath) ~
        put {
          entity(as[String]) { data =>
            import Loan._

            loan(new FileWriter(filePath)) to { _.write(data) }
            complete { "ok " }
          }
        }
      }
    } ~
    path("") {
      getFromFile("webapp/index.html")
    } ~
    getFromDirectory("webapp")
}
