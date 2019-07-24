package models

object Item extends ModelCompanion[Item] {
  protected def dbTable: DatabaseTable[Item] = Database.items

    def apply(name: String, description: String, price: Float,
     providerUsername: String): Item =
      new Item(name, description, price, providerUsername)

  def existsItemByProvider(itemName: String, provider: Int): Boolean =
    filter(Map("providerId" -> provider))
    .exists(m => (Map("name" -> itemName)).toSet.subsetOf(m.toMap.toSet))

  def itemId(itemName: String, provider: String): Int =
      filter(Map("providerUsername" -> provider))
      .filter(m => (Map("name" -> itemName)).toSet.subsetOf(m.toMap.toSet))
      .head.toMap.get("id").get.toString.toInt

  private[models] def apply(jsonValue: JValue): Item = {
    val value = jsonValue.extract[Item]
    value._id = (jsonValue \ "id").extract[Int]
    value.save()
    value
  }
 }

class Item(val name: String, val description: String, var price: Float,
 val providerUsername: String) extends Model[Item]{

  protected def dbTable: DatabaseTable[Item] = Item.dbTable

  val providerId: Any = Provider.idByUsername(providerUsername)

  def priceByNameProvider(n: String, p: String): Float =
     if (n == name && p == providerUsername){ price } else 0

  override def toMap: Map[String, Any] = Map("id" ->  id, "name" -> name,
   "description" -> description, "price" -> price, "providerId" -> providerId, "providerUsername" -> providerUsername)

  override def toString: String =
    s"name: $name description: $description price: $price providerId: $providerId"
}
