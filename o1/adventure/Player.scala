package o1.adventure

import scala.collection.mutable.Map
import scala.math._
import scala.util.Random


/** A `Player` object represents a player character controlled by the real-life user of the program.
  *
  * A player object's state is mutable: the player's location and possessions can change, for instance.
  *
  * @param startingArea  the initial location of the player */
class Player(startingArea: Area) {

  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false
  private var inventaario = Map[String, Item]()

  var levelingSystem = Vector(0.to(8) -> 1, 9.to(47) -> 2, 48.to(112) -> 3, 113.to(202) -> 4, 203.to(204) -> 5) // Experience required for levels
  var levelStats = Map(1 -> (5, 20), 2 -> (7, 25), 3 -> (9, 32), 4 -> (12, 40), 5 -> (17, 50))                   // Stats for different levels; level -> (attackStrength, maxHealth)

  private var attackStrength = Vector(levelStats(level)._1-1, levelStats(level)._1, levelStats(level)._1+1)
  private var maxHealth = levelStats(level)._2
  private var currentHealth = levelStats(level)._2
  var experience = 0

  var randomizerNumber = 0 //This number is increased by 1 every turn
  var randomizerEngine = new Random

  def randomizer(toRandom: Int) = {
    var randomNumbers = LazyList.continually(randomizerEngine.nextInt(toRandom))
    randomNumbers(randomizerNumber)
  }

  def level = {  // Checking level

    if(experience > 204) {
      5
    }
    else {
    levelingSystem.filter(n=> n._1.contains(experience)).head._2
    }
  }

  def levelUp(level: Int) = {
    attackStrength = Vector(levelStats(level)._1-1, levelStats(level)._1, levelStats(level)._1+1) // Update attackStrength
    var oldMaxHealth = maxHealth
    maxHealth = levelStats(level)._2                                                              // Update maxHealth
    currentHealth += maxHealth - oldMaxHealth                                                     // Add increase in maxHealth to current health
    if(level == 5) {
      "Congratulations! You have reached the maximum level of " + level + "!"
    }
    else {
    "Congratulations! You are now level " + level + "!"
    }
  }

  def isDead = currentHealth == 0




  def attack = {
    if(currentLocation.hasEnemy) {

      val attackInfo = currentLocation.enemy.get.takeDamage(attackStrength(randomizer(attackStrength.size))) //Deal damage to enemy and return return string with info about attack
      if(currentLocation.enemy.get.isDead){

        var info = ""
        var currentLevel = level
        experience += currentLocation.enemy.get.experience

          if(currentLocation.enemy.get.hasItem) {
          currentLocation.addItem(currentLocation.enemy.get.itemDrop.get)
          info += currentLocation.removeEnemy
             if(level > currentLevel) {
          info += "\n\n" + levelUp(level)
             }
          }
          else {
          info += currentLocation.removeEnemy
             if(level > currentLevel) {
              info += "\n\n" + levelUp(level)
             }

          }
        info += "\n\n" + currentLocation.fightDescription
        info

      }
      else {
        attackInfo + "\n" + currentLocation.enemy.get.name + " now has " + currentLocation.enemy.get.healthRemaining + " health points left." + "\n\n" + currentLocation.enemy.get.name + takeDamage(currentLocation.enemy.get.currentAttack(randomizer(currentLocation.enemy.get.currentAttack.size))) + "\n\n\n" + "You now have " + currentHealth + " health left."
      }

    }
    else {
      "There is no enemy to fight!"
    }

  }

  def use(itemName: String) = {
    if(inventaario.contains(itemName)) {
      inventaario(itemName).useCase match {
        case "healing" => healing(inventaario(itemName))
        case "sleepy" => sleepy(inventaario(itemName))
        case "achievement" => "Hmm... better just keep it for when I get home."
      }
    }
    else {
      "You don't have this item."
    }
  }

  def takeDamage(amount: Int) = {
    var takenDamage = min(currentHealth, amount)
    currentHealth = max(currentHealth - amount, 0)
    " deals you " + takenDamage + " of damage."

  }

  def check(thingToCheck: String) = {
    thingToCheck match {
      case "health" => "You currently have " + currentHealth + "/" + maxHealth + " health."
      case "attack" => "Your attack strength is currently " + attackStrength.head + "-" + attackStrength.reverse.head
      case "level" =>  if(level == 5) "You are level " + level + " (max level)" else "You are level " + level
      case "exits" => "Exits available: " + currentLocation.neighbors.keys.mkString(" ")
      case "area" => currentLocation.name
      case other => "No clue mate."
    }
  }

  def healing(healingItem: Item) = {
    var amount = healingItem.value
    var amountOfHealing = min(maxHealth - currentHealth, amount)
    currentHealth = min(currentHealth + amount, maxHealth)
    inventaario -= healingItem.name

    "You regain " + amountOfHealing + " health." + "\nYou now have " + currentHealth + "/" + maxHealth + " health."
  }

  def sleepy(sleepDart: Item) = {
    if(currentLocation.enemy.isDefined){
    currentLocation.enemy.get.currentAttack = currentLocation.enemy.get.currentAttack.map(n => n/sleepDart.value)
    inventaario -= sleepDart.name
    "Your opponent feels drowsy and is now significantly weaker!"
    }
    else {
      "Better save it for when you really need it!"
    }
  }

  def get(itemName: String) = {
    if(currentLocation.contains(itemName)) {
      var esine = currentLocation.removeItem(itemName).get
      inventaario += esine.name -> esine
      var onnistunu = "You pick up the " + itemName
      onnistunu
    }
    else {
    var teksti = "There is no " + itemName + " here to pick up."
    teksti
    }
  }

  def inventory = {
    if(inventaario.isEmpty) {
      "You are empty-handed."
    }
    else {
      "You are carrying: \n" + inventaario.keys.mkString("\n")
    }
  }

  def drop(itemName: String) = {
    if(inventaario.contains(itemName)) {
    currentLocation.addItem(inventaario(itemName))
    inventaario.remove(itemName)
    var text = "You drop the " + itemName + "."
    text
    }
    else {
      "You don't have that!"
    }

  }

  def examine(itemName: String) = {
    if(inventaario.contains(itemName)) {
      "You look closely at the " + itemName + ".\n" + inventaario(itemName).description
    }
    else {
      "If you want to examine something, you need to pick it up first."
    }
  }

  def has(itemName: String) = inventaario.contains(itemName)




  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven


  /** Returns the current location of the player. */
  def location = this.currentLocation




  /** Attempts to move the player in the given direction. This is successful if there
    * is an exit from the player's current location towards the direction name. Returns
    * a description of the result: "You go DIRECTION." or "You can't go DIRECTION." */
  def go(direction: String) = {
    val destination = this.location.neighbor(direction)
    this.currentLocation = destination.getOrElse(this.currentLocation)

    if (destination.isDefined) {
      //Spawns a new enemy in the location in case the Spawn enemy setting is turned on for the area and the area is empty
      if(destination.get.spawnEnemies && !destination.get.hasEnemy) {

         destination.get.addEnemy(destination.get.possibleEnemies.get(randomizer(destination.get.possibleEnemies.get.size)))
      }

      if(destination.get.name == "The Chopper!" && inventaario.contains("key")) { // To prevent printing the description when win condition is filled
       "You go " + direction + "." + "\n\n" + currentLocation.name
      }
      else {
       "You go " + direction + "." + "\n\n" + currentLocation.name + "\n\n" + currentLocation.fullDescription

      }


    }
    else {
      "You can't go " + direction + "."
    }
  }




  /** Signals that the player wants to quit the game. Returns a description of what happened within
    * the game as a result (which is the empty string, in this case). */
  def quit() = {
    this.quitCommandGiven = true
    ""
  }

  def help = {
    "Move around in the game using the command go followed by some available direction.\n" + "Areas in the game contain enemies of various difficulty which you can fight using the attack command.\nWeaker enemies respawn ones you re-enter certain areas, train against them to get xp.\nEnemies can block a certain direction or be carrying items which they drop ones killed. \nFighting enemies will also give you experience which will increase your level, causing your attack strength and max health to increase.\nYou win the game by getting to safety.\n\nFighting Mechanics\nWhen attacking you will strike first and deal an amount of damage depending on your attack strength as well as a random factor,\nthe enemy strikes second so therefore if you kill the enemy with one strike you will not take any damage.\n\nItems\nThe game contains various items which can be used to help you fight the enemies, regain health or are otherwise useful.\nPick up items with the command get itemname and use them with the command use itemname.\nItems can be examined ones they are in your inventory, for example examining healing items will tell you how many hp they will heal you.\nUse command inventory to check your inventory\n\nChecking Status\nYou can get various information about the status of your player.\ncheck attack, your attack strength; check health, your current health; check level, your current level\ncheck area: the name of the area, check exits: available exits"
  }


  /** Returns a brief description of the player's state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name


}


