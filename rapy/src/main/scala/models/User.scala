package models

abstract class User{
  val username: String
  val locationName: String
  var balance: Float
  val locationId: Any

  def existingUser(uservalue: Any): Boolean =
    Provider.exists("username", uservalue) && Consumer.exists("username", uservalue)

  def decBalance(price: Float): Unit = {
    balance = balance - price
  }

  def incBalance(price: Float): Unit = {
  	balance = balance + price
  }
}
