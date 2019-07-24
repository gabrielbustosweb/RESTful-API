package models

object Order extends ModelCompanion[Order] {
  protected def dbTable: DatabaseTable[Order] = Database.orders

    def apply(providerUsername: String, consumerUsername: String, items: List[(String, Int)]): Order =
      new Order(providerUsername, consumerUsername, items)

  private[models] def apply(jsonValue: JValue): Order = {
    val value = jsonValue.extract[Order]
    value._id = (jsonValue \ "id").extract[Int]
    value.save()
    value
  }
 }

class Order(val providerUsername: String, val consumerUsername: String, val items: List[(String, Int)]) extends Model[Order]{
  protected def dbTable: DatabaseTable[Order] = Order.dbTable

  protected var _status: String = "paid"

  def deliverOrder: Unit = { _status = "delivered" }

  var purchaseAmount: Float = items.map(a => (Item.find(Item.itemId(a._1, providerUsername)).get.price.toInt) * a._2 ).sum

  val consumerId: Int = Consumer.idByUsername(consumerUsername)

  val providerId: Int = Provider.idByUsername(providerUsername)

  def balances: Unit = {
    val consumer = Consumer.find(consumerId).get
    consumer.decBalance(purchaseAmount)
    consumer.save()
    val provider = Provider.find(providerId).get
    provider.incBalance(purchaseAmount)
    provider.save()
  }

  override def toMap: Map[String, Any] = Map("id" ->  id,
   "providerUsername" -> providerUsername, "consumerUsername" -> consumerUsername,
   "items" -> items, "status" -> _status, "purchaseAmount" -> purchaseAmount)

  override def toString: String = s"providerUsername: $providerUsername consumerUsername: $consumerUsername"
}
