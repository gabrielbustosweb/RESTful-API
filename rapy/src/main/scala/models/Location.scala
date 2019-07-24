package models

object Location extends ModelCompanion[Location] {
  protected def dbTable: DatabaseTable[Location] = Database.locations

  def apply(name: String, coordX: Int, coordY: Int): Location =
     new Location(name, coordX, coordY)

  def getLocationId(locationName: String): Any =
    Location.filter(Map("name" -> locationName)).head.toMap.get("id").get

  def availableLocation(consLocationId: Int, provLocationId: Int, radio: Int): Boolean ={
    val coordAx: Int = Location.find(consLocationId).get.coordX.toString.toInt
    val coordBx: Int = Location.find(provLocationId).get.coordX.toString.toInt
    val coordAy: Int = Location.find(consLocationId).get.coordY.toString.toInt
    val coordBy: Int = Location.find(provLocationId).get.coordY.toString.toInt
    val distance: Int = Math.abs(coordAx - coordBx) + Math.abs(coordAy - coordBy)
    if (distance <= radio){
      true
    } else false
  }


   private[models] def apply(jsonValue: JValue): Location = {
     val value = jsonValue.extract[Location]
     value._id = (jsonValue \ "id").extract[Int]
     value.save()
     value
   }
 }

class Location(val name: String, val coordX: Int, val coordY: Int) extends Model[Location] {
  protected def dbTable: DatabaseTable[Location] = Location.dbTable

  override def toMap: Map[String, Any] = Map("id" -> id, "name" -> name,
   "coordX" -> coordX, "coordY" -> coordY)

  override def toString: String = s"Location: $name"
}
