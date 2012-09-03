package algorithm

import net.liftweb.http.{LiftSession, S}
import service.User
import bootstrap.liftweb.CurrentUser
import net.liftweb.common.{Empty, Full}
import data.mongo.Location
import algorithm.Labeller._
import algorithm.Labeller.Segmentation
import net.liftweb.util.StringHelpers

/**
 * Created with IntelliJ IDEA.
 * User: zilske
 * Date: 9/3/12
 * Time: 2:42 PM
 * To change this template use File | Settings | File Templates.
 */
class LearnScript extends App {

  new bootstrap.liftweb.Boot().boot
  val session = new LiftSession("", StringHelpers.randomString(20), Empty)
  S.initIfUninitted(session) {
    val user = new User("111742407880819503242")
    CurrentUser.set(Full(user)) // That's me! {

  }

}
