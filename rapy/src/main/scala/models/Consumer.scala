package models

object Consumer extends ModelCompanion[Consumer] {
  protected def dbTable: DatabaseTable[Consumer] = Database.consumers

    def apply(username: String, locationName: String): Consumer =
      new Consumer(username, locationName)

  def idByUsername(username: String): Int =
    filter(Map("username" -> username)).head.toMap.get("id").get.toString.toInt

  private[models] def apply(jsonValue: JValue): Consumer = {
    val value = jsonValue.extract[Consumer]
    value._id = (jsonValue \ "id").extract[Int]
    value.save()
    value
  }
 }

class Consumer(val username: String,
 val locationName: String) extends User with Model[Consumer]{
  protected def dbTable: DatabaseTable[Consumer] = Consumer.dbTable

  var balance: Float = 0

  val locationId = Location.getLocationId(locationName)

  override def toMap: Map[String, Any] = Map("id" ->  id, "username" -> username,
   "balance" -> balance, "locationId" -> locationId, "locationName" -> locationName)

  override def toString: String = s"username: $username locationId: $locationId balance: $balance"
}
