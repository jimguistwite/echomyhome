package controllers

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import play.api._
import play.api.libs.json._
import play.api.mvc._
import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.concurrent.Execution.Implicits._
import org.slf4j.LoggerFactory
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

class Application extends Controller {

  val logger = LoggerFactory.getLogger(classOf[Application])

  case class EchoApplication(applicationId: String)
  case class User(userId: String)
  case class Session(sessionId: String, application: EchoApplication, user: User, isnew: Boolean)
  case class Request(requestType: String, requestId: String, timestamp: DateTime, intent: Option[Intent] = None, reason: Option[String] = None)

  case class Intent(intentName: String, slot: JsObject) {
    logger.debug("parse slot string {}", slot)
    val slots = slot.values.map(v=>{
      val jso = v.as[JsObject]
      Slot((jso \ "name").as[String],(jso \ "value").asOpt[String])
    })
  }

  case class Slot(name: String, value: Option[String])

  case class EchoRequest(session: Session, request: Request)

  val dateFormat = "yyyy-MM-dd'T'HH:mm:ssZ"

  val jodaDateReads = Reads[DateTime](js =>
    js.validate[String].map[DateTime](dtString =>
      DateTime.parse(dtString, DateTimeFormat.forPattern(dateFormat))
    )
  )
  implicit val userReads: Reads[User] = (__ \ "userId").read[String].map { name => User(name) }
  implicit val echoApplicationReads: Reads[EchoApplication] = (__ \ "applicationId").read[String].map { name => EchoApplication(name) }

  implicit val sessionReads: Reads[Session] = (
    (JsPath \ "sessionId").read[String] and
      (JsPath \ "application").read[EchoApplication] and
      (JsPath \ "user").read[User] and
      (JsPath \ "new").read[Boolean]
    )(Session.apply _)

  implicit val intentReads: Reads[Intent] = (
    (JsPath \ "name").read[String] and
      (JsPath \ "slots").read[JsObject]
    )(Intent.apply _)

  implicit val requestReads: Reads[Request] = (
    (JsPath \ "type").read[String] and
      (JsPath \ "requestId").read[String] and
      (JsPath \ "timestamp").read[DateTime](jodaDateReads) and
      (JsPath \ "intent").readNullable[Intent] and
      (JsPath \ "reason").readNullable[String]
    )(Request.apply _)

  implicit val echoRequestReads: Reads[EchoRequest] = (
    (JsPath \ "session").read[Session] and
      (JsPath \ "request").read[Request]
    )(EchoRequest.apply _)



  def index = Action {
    Ok("Hello")
  }

  /*
    {
"version": "1.0",
"sessionAttributes": {
"supportedHoriscopePeriods": {
  "daily": true,
  "weekly": false,
  "monthly": false
}
},
"response": {
"outputSpeech": {
  "type": "PlainText",
  "text": "Today will provide you a new learning opportunity.  Stick with it and the possibilities will be endless. Can I help you with anything else?"
},
"card": {
  "type": "Simple",
  "title": "Horoscope",
  "content": "Today will provide you a new learning opportunity.  Stick with it and the possibilities will be endless."
},
"reprompt": {
  "outputSpeech": {
    "type": "PlainText",
    "text": "Can I help you with anything else?"
  }
},
"shouldEndSession": false
}
}
     */

  def echo = Action.async(parse.json) { request =>
    logger.debug(s"process request ${request.body}")

    try {
      val echorequest = request.body.as[EchoRequest]
      logger.debug("parsed json request!!! {}", echorequest)
      echorequest.request.intent match {
        case None =>
          Future(Ok(Json.obj("version"->"1.0",
            "response"->Json.obj(
              "shouldendsession"->false,
              "outputSpeech"->outputSpeech("I don't really know how to do that."),
              "card"->card("MyHome", "I don't know how to do what you've asked me to.")))))
        case Some(intent) =>
          intent.intentName match {
            case "MyHomeIntent" =>
              Future(Ok(Json.obj("version"->"1.0",
                "response"->Json.obj(
                  "shouldendsession"->true,
                  "outputSpeech"->outputSpeech("Sure, I know how to do that."),
                  "card"->card("MyHome", "Sure, let me take care of that for you.")))))
            case _ =>
              Future(Ok(Json.obj("version"->"1.0",
                "response"->Json.obj(
                  "shouldendsession"->false,
                  "outputSpeech"->outputSpeech("I don't really know how to do that."),
                  "card"->card("MyHome", "I don't know how to do what you've asked me to.")))))
          }
      }
    } catch {
      case (e: Exception) =>
        logger.error("caught exception", e)
        Future(Ok(Json.obj("version"->"1.0",
          "response"->Json.obj("outputSpeech"->outputSpeech("I was unable to do that due to an error"),
            "card"->card("MyHome", "See you later.")))))
    }
  }

  def card(title: String, content: String): Map[String,String] = {
    Map("type"->"Simple", "title"->title, "content"->content)
  }
  def outputSpeech(txt: String): Map[String,String] = {
    Map("type"->"PlainText", "text"->txt)
  }
}
