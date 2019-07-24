package models

object Provider extends ModelCompanion[Provider] {
  protected def dbTable: DatabaseTable[Provider] = Database.providers

    def apply(username: String, locationName: String, storeName: String,
     maxDeliveryDistance: Int): Provider =
      new Provider(username, locationName, storeName, maxDeliveryDistance)

    def idByUsername(username: String): Int =
      filter(Map("username" -> username)).head.toMap.get("id").get.toString.toInt

  private[models] def apply(jsonValue: JValue): Provider = {
    val value = jsonValue.extract[Provider]
    value._id = (jsonValue \ "id").extract[Int]
    value.save()
    value
  }
 }

class Provider(val username: String, val locationName: String,
 val storeName: String, val maxDeliveryDistance: Int) extends User with Model[Provider]{
  protected def dbTable: DatabaseTable[Provider] = Provider.dbTable

  var balance: Float = 0

  val locationId = Location.getLocationId(locationName)

  override def toMap: Map[String, Any] = Map("id" ->  id, "username" -> username,
   "locationId" -> locationId ,"balance" -> balance, "storeName" -> storeName,
    "maxDeliveryDistance" -> maxDeliveryDistance, "locationName" -> locationName)

  override def toString: String = s"id: $id username: $username locationId: $locationId balance: $balance"
}
