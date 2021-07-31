package o1.adventure

import scala.collection.mutable.Map

/** The class `Area` represents locations in a text adventure game world. A game world
  * consists of areas. In general, an "area" can be pretty much anything: a room, a building,
  * an acre of forest, or something completely different. What different areas have in
  * common is that players can be located in them and that they can have exits leading to
  * other, neighboring areas. An area also has a name and a description.
  * @param name         the name of the area
  * @param description  a basic description of the area (typically not including information about items) */
abstract class Area(var name: String, var description: String) {

  //Settings
  var spawnEnemies: Boolean
  var possibleEnemies: Option[Vector[Enemy]]

  var neighbors = Map[String, Area]()
  private var items = Map[String, Item]()
  var enemy: Option[Enemy] = None
  private var blocked: Option[(String, Area)] = None
  var enemyItemHolder: Option[Item] = None



  def hasEnemy = enemy.isDefined

  def removeEnemy = {

   var info = "You kill " + enemy.get.name + " and receive " + enemy.get.experience + " XP"

   if(enemy.get.hasItem) {
     info += "\n" + enemy.get.name + " drops " + enemy.get.itemDrop.get.name
   }


   if(blocked.isDefined) {
     info += "\n\n" + "Exit " + blocked.get._1 + " is no longer blocked!"
     releaseBlock

   }

    enemy = None



    info
   }



  def addEnemy(enemyToAdd: Enemy) = {
    enemy = Some(enemyToAdd)
    if(enemyToAdd.hasItem) {
      enemyItemHolder = enemyToAdd.itemDrop
    }
  }

  def addBlockedNeighbor(direction: String) = {
    blocked = Some((direction, neighbors(direction)))
    neighbors = neighbors -= direction

  }

  def releaseBlock = {
    var direction = blocked.get._1
    var alue = blocked.get._2

    neighbors = neighbors += direction -> alue

  }

  def addItem(item: Item) = {
    items += item.name -> item
  }


  def contains(itemName: String) = {
    items.contains(itemName)
  }

  def removeItem(itemName: String) = {
    var removedItem = items.get(itemName)
    items.remove(itemName)
    removedItem
  }

  /** Returns the area that can be reached from this area by moving in the given direction. The result
    * is returned in an `Option`; `None` is returned if there is no exit in the given direction. */
  def neighbor(direction: String) = this.neighbors.get(direction)


  /** Adds an exit from this area to the given area. The neighboring area is reached by moving in
    * the specified direction from this area. */
  def setNeighbor(direction: String, neighbor: Area) = {
    this.neighbors += direction -> neighbor
  }


  /** Adds exits from this area to the given areas. Calling this method is equivalent to calling
    * the `setNeighbor` method on each of the given direction--area pairs.
    * @param exits  contains pairs consisting of a direction and the neighboring area in that direction
    * @see [[setNeighbor]] */
  def setNeighbors(exits: Vector[(String, Area)]) = {
    this.neighbors ++= exits
  }


  /** Returns a multi-line description of the area as a player sees it. This includes a basic
    * description of the area as well as information about exits and items. The return
    * value has the form "DESCRIPTION\n\nExits available: DIRECTIONS SEPARATED BY SPACES".
    * The directions are listed in an arbitrary order. */
  def fullDescription = {
    var exitList = "\n\nExits available: " + this.neighbors.keys.mkString(" ")
    val itemList = "\nItems to pick up: "+ items.keys.mkString(" ")
    var test = enemy
    val vihollinen = test match {
      case Some(badguy) => "\n\nOh no! " + badguy.name
      case None => "\n\nNo enemies around."
    }

    if(blocked.isDefined) {
      exitList = "\n\nAn exit appears to be blocked!" + "\n\nExits available: " + this.neighbors.keys.mkString(" ")
    }


    if(items.isEmpty) {
      this.description + exitList + vihollinen
    }
    else {
       this.description + itemList + exitList + vihollinen
    }
  }

  def fightDescription = {
    val exitList = "\n\nExits available: " + this.neighbors.keys.mkString(" ")
    val itemList = "\nItems to pick up: "+ items.keys.mkString(" ")
     var test = enemy
    val vihollinen = test match {
      case Some(badguy) => "\n\nOh no! " + badguy.name
      case None => "\n\nNo enemies around."
    }

     if(items.isEmpty) {
      exitList + vihollinen
    }
    else {
      itemList + exitList + vihollinen
    }

  }


  /** Returns a single-line description of the area for debugging purposes. */
  override def toString = this.name + ": " + this.description.replaceAll("\n", " ").take(150)



}

class RandomEnemyArea1(name: String, description: String) extends Area(name, description) {
  var spawnEnemies = true
  var possibleEnemies: Option[Vector[Enemy]] = Some(Vector(new Enemy1("Creeper"), new Enemy1("Biter"), new Enemy1("Lurker")))
}

class RandomEnemyArea2(name: String, description: String) extends Area(name, description) {
  var spawnEnemies = true
  var possibleEnemies: Option[Vector[Enemy]] = Some(Vector(new Enemy2("Roamer"), new Enemy2("Walker")))
}

class RandomEnemyArea3(name: String, description: String) extends Area(name, description) {
  var spawnEnemies = true
  var possibleEnemies: Option[Vector[Enemy]] = Some(Vector(new Enemy3("Rotter"), new Enemy3("Dragger")))
}

class NoEnemiesArea(name: String, description: String) extends Area(name, description) {
  var spawnEnemies = false
  var possibleEnemies: Option[Vector[Enemy]] = None
}

class BossArea(name: String, description: String) extends Area(name, description) {
  var spawnEnemies = false
  var possibleEnemies: Option[Vector[Enemy]] = None
}

