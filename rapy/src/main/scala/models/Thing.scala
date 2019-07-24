package models

import cask._
import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

case class Thing(name: String, amount: Int){
  def toTuple: (String, Int) = (name, amount)

}
object Thing{
  implicit val rw: RW[Thing] = macroRW
}
