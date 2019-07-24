package app

import cask._
import models._

object RestfulAPIServer extends MainRoutes  {
  override def host: String = "0.0.0.0"
  override def port: Int = 4000

  @get("/")
  def root(): Response = {
    JSONResponse("Ok")
  }

  @get("/api/locations")
  def locations(): Response = {
    JSONResponse(Location.all.map(location => location.toMap))
  }

  @postJson("/api/locations")
  def locations(name: String, coordX: Int, coordY: Int): Response = {
    if (Location.exists("name", name)) {
      return JSONResponse("Existing location", 409)
    }
    val location = Location(name, coordX, coordY)
    location.save()
    JSONResponse(location.id)
  }
  @get("/api/consumers")
  def consumers(): Response = {
    JSONResponse(Consumer.all.map(consumer => consumer.toMap))
  }

  @postJson("/api/consumers")
  def consumers(username: String, locationName: String): Response = {
    if (Consumer.exists("username", username)
    || Provider.exists("username", username)){
      return JSONResponse("Existing username", 409)
    }
    if (Location.exists("name", locationName)){
      val consumer = Consumer(username, locationName)
      consumer.save()
      JSONResponse(consumer.id)
    }
    else
      return JSONResponse("non existing location", 404)
  }

  @get("/api/providers")
  def providers(locationName: String = ""): Response ={
    locationName match {
      case "" => JSONResponse(Provider.all.map(provider => provider.toMap))
      case locationName if Location.exists("name", locationName) =>
       JSONResponse(Provider.filter(Map(
         "locationId" -> Location.getLocationId(locationName
         )))
      .map(provider => provider.toMap))
      case _ => return JSONResponse("non existing location", 404)
    }
  }

  @postJson("/api/providers")
  def providers(username: String, locationName: String,
   storeName: String, maxDeliveryDistance: Int): Response ={
    if (Consumer.exists("username", username)
    || Provider.exists("username", username)){
      return JSONResponse("Existing username", 409)
    }
    else if (Provider.exists("storeName", storeName)){
      return JSONResponse("Existing storeName", 409)
    }
    else if (maxDeliveryDistance <= 0){
      return JSONResponse("negative maxDeliveryDistance", 400)
    }
    else if (Location.exists("name", locationName)){
      val provider =
        Provider(username, locationName, storeName, maxDeliveryDistance)
      provider.save()
      JSONResponse(provider.id)
    }
    else
      return JSONResponse("non existing location", 404)
  }

  @post("/api/users/delete/:username")
  def deleteUser(username: String): Response ={
    if (Consumer.exists("username", username)){
      Consumer.delete(Consumer.idByUsername(username))
      JSONResponse("Ok")
    }
    else if (Provider.exists("username", username)){
      Provider.delete(Provider.idByUsername(username))
      JSONResponse("Ok")
    }
    else
      return JSONResponse("non existing user",404)
  }

  @get("/api/items")
  def items(providerUsername: String = ""): Response ={
    val mapOfProvider: Map[String, Any] =
      Map("providerUsername" -> providerUsername)
    if (providerUsername == ""){
      JSONResponse(Item.all.map(item => item.toMap))
    }
    else
      JSONResponse(Item.filter(mapOfProvider).map(item => item.toMap))
  }

  @postJson("/api/items")
  def items(name: String, description: String, price: Float,
   providerUsername: String): Response ={
    if (Provider.exists("username", providerUsername)){
      if (Item.existsItemByProvider(name,
         Provider.idByUsername(providerUsername))
      ){
        return JSONResponse("existing item for provider", 409)
      }
      if (price <= 0){
        return JSONResponse("negative price", 400)
      }
      val item = Item(name, description, price, providerUsername)
      item.save()
      JSONResponse(item.id)
    }
    else
      return JSONResponse("non existing provider", 404)
  }

  @cask.post("/api/items/delete/:id")
  def removeItem(id: Int): Response ={
    if (Item.exists("id", id)){
      Item.delete(id)
      JSONResponse("Ok")
    }
    else
      return JSONResponse("non existing item", 404)
  }

  @get("/api/orders/username/:username")
  def orders(username: String): Response ={
    if (Consumer.exists("username", username)){
      JSONResponse(
        Order.filter(Map("consumerUsername" -> username))
        .map(order => order.toMap
        ))
    }
    else if (Provider.exists("username", username)){
      JSONResponse(
        Order.filter(Map("providerUsername" -> username))
        .map(order => order.toMap
        ))
    }
    else
      return JSONResponse("non existing user", 404)
  }

  @get("/api/orders/detail/:id")
  def detail(id: Int): Response = {
    if (Order.exists("id", id)){
      JSONResponse(Order.find(id).map(order => order.toMap))
    }
    else
      return JSONResponse("non existing order", 404)
  }

  @postJson("/api/orders")
  def orders(providerUsername: String, consumerUsername: String,
   items: List[Thing]): Response =
  {
    items match {
      case List() => JSONResponse("items is empty", 404)
      case items if !(Provider.exists("username", providerUsername)) =>
        JSONResponse("non existing provider", 404)
      case items if !(Consumer.exists("username", consumerUsername)) =>
        JSONResponse("non existing consumer", 404)
      case items if !(Location.availableLocation(
        Provider.find(
          Provider.idByUsername(providerUsername))
          .get.locationId.toString.toInt,
        Consumer.find(
          Consumer.idByUsername(consumerUsername))
          .get.locationId.toString.toInt,
        Provider.find(
          Provider.idByUsername(providerUsername))
          .get.maxDeliveryDistance.toString.toInt
      )) =>
        JSONResponse("provider no delivered for you location",404)
      case items if (items.map(m =>
        (Item.existsItemByProvider(m.name,
        Provider.idByUsername(providerUsername))))
        .forall(a => a)) =>
        val order =
          Order(providerUsername, consumerUsername, items.map(m => m.toTuple))
        order.save()
        JSONResponse(order.id)
      case _ =>
        JSONResponse("non existing item for provider", 404)

    }

  }

  @cask.post("/api/orders/delete/:id")
  def removeOrder(id: Int): Response ={
    if (Order.exists("id", id)){
      Order.delete(id)
      JSONResponse("Ok")
    }
    else
      return JSONResponse("non existing order", 404)
  }

  @cask.post("/api/orders/deliver/:id")
  def deliver(id: Int): Response ={
    if (Order.exists("id", id)){
      val order = Order.find(id).get
      order.deliverOrder
      order.save()
      JSONResponse("Ok")
    }
    else
      return JSONResponse("non existing order", 404)
  }

  override def main(args: Array[String]): Unit = {
    System.err.println("\n " + "=" * 39)
    System.err.println(s"| Server running at http://$host:$port ")

    if (args.length > 0) {
      val databaseDir = args(0)
      Database.loadDatabase(databaseDir)
      System.err.println(s"| Using database directory $databaseDir ")
    } else {
      Database.loadDatabase()  // Use default location
    }
    System.err.println(" " + "=" * 39 + "\n")

    super.main(args)
  }

  initialize()
}
