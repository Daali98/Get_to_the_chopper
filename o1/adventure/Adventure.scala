package o1.adventure


/** The class `Adventure` represents text adventure games. An adventure consists of a player and
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game.
  *
  * N.B. This version of the class has a lot of "hard-coded" information which pertain to a very
  * specific adventure game that involves a small trip through a twisted forest. All newly created
  * instances of class `Adventure` are identical to each other. To create other kinds of adventure
  * games, you will need to modify or replace the source code of this class. */
class Adventure {

  /** The title of the adventure game. */
  val title = "Get to the chopper!"

  private val first = new NoEnemiesArea("Home", "Home sweet home.. no more. Can't just lay around here all day anymore.")
  private val second = new RandomEnemyArea1("The Mall", "So quiet here, all you can hear are the light bulbs crackling...")
  private val third = new RandomEnemyArea1("The School", "No sign of any kids. Just the sound of swings swaying in the wind...")
  private val fourth = new RandomEnemyArea1("The Grocery Store", "Rotten food remains laying all over the floor...")
  private val fifth = new BossArea("The Hospital", "Blood and corpses are everywhere. Feels like this is where people come to die, not be saved...")
  private val sixth = new NoEnemiesArea("Nana's Cottage", "The rain has started to fall.")
  private val seven = new RandomEnemyArea2("The Train Station", "Doors left open. This train seems to be broken down.")
  private val eight = new RandomEnemyArea2("Wildwood", "It's getting dark and cold outside.")
  private val ten = new BossArea("Old Farm", "All plants are dead. Nothing survived.")
  private val eleven = new NoEnemiesArea("The Bunker", "Pitch dark in here, but still safer than outside.")
  private val tvelwe = new RandomEnemyArea3("Road near the Airport", "It's dangerous out here in the open at nigthtime \nbut you feel like safety might be close.")
  private val fourteen = new RandomEnemyArea3("Old Factory", "Lots of broken down machinery. Smells really bad.")
  private val fifthteen = new NoEnemiesArea("Warehouse", "Completely empty and with locks, seems pretty safe.")
  private val sixteen = new BossArea("Military Camp", "Broken down tents. Could be something useful around here.")
  private val seventeen = new BossArea("The Airport", "No planes, no people and the smell of blood stronger than anywhere else...")
  private val eightteen = new NoEnemiesArea("The Chopper!", "Your ticket to safety!\nBut where is the key??")


     first.setNeighbors(Vector("north" -> second                                                                          ))
    second.setNeighbors(Vector("north" -> fifth,       "east" -> fourth,     "south" -> first,      "west" -> third       ))
     third.setNeighbors(Vector(                        "east" -> second                                                   ))
    fourth.setNeighbors(Vector(                                                                     "west" -> second      ))
     fifth.setNeighbors(Vector("north" -> sixth,                             "south" -> second                            ))
     sixth.setNeighbors(Vector("north" -> seven,       "east" -> eight,      "south" -> fifth                             ))
     seven.setNeighbors(Vector(                                              "south" -> sixth                             ))
     eight.setNeighbors(Vector("north" -> ten,                                                      "west" -> sixth       ))
       ten.setNeighbors(Vector("north" -> eleven,                            "south" -> eight                             ))
    eleven.setNeighbors(Vector("north" -> seventeen,   "east" -> fourteen,   "south" -> ten,        "west" -> tvelwe      ))
    tvelwe.setNeighbors(Vector("north" -> eightteen,   "east" -> eleven                                                   ))
  fourteen.setNeighbors(Vector("north" -> fifthteen,                                                "west" -> eleven      ))
 fifthteen.setNeighbors(Vector(                        "east" -> sixteen,    "south" -> fourteen                          ))
   sixteen.setNeighbors(Vector(                                                                     "west" -> fifthteen   ))
 seventeen.setNeighbors(Vector(                                              "south" -> eleven                            ))
 eightteen.setNeighbors(Vector(                                              "south" -> tvelwe                            ))

  val goalDestination = eightteen


  var rageWalker = new RageWalker("RAGEWALKER")
  var mordath = new Mordath("MORDATH")
  var skinrender = new Skinrender("SKINRENDER")
  var corpseBreath = new CorpseBreath("CORPSEBREATH")

  fifth.addEnemy(rageWalker)
  ten.addEnemy(mordath)
  sixteen.addEnemy(skinrender)
  seventeen.addEnemy(corpseBreath)


  second.addItem(new Potion("burana", "Heals 5 HP", 5))
  seven.addItem(new Potion("white pill", "Heals 7 HP", 7))
  tvelwe.addItem(new Potion("vodka bottle", "Heals 15 HP", 15))


  fifth.addBlockedNeighbor("north")
  ten.addBlockedNeighbor("north")

  /** The character that the player controls in the game. */
  val startLocation = first
  val player = new Player(startLocation)

  /** The number of turns that have passed since the start of the game. */
  var turnCount = 0


  /** Determines if the adventure is complete, that is, if the player has won. */
  def isComplete = (this.player.location == this.goalDestination) && player.has("key")

  /** Determines whether the player has won, lost, or quit, thereby ending the game. */
  def isOver = this.isComplete || this.player.hasQuit || this.player.isDead

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage = "\n\nThe apocalypse has started!\nYou're not safe here, find your way to safety!\n\nUse the help command if it's your first time playing." + "\n\n" + startLocation.name + "\n" + startLocation.fullDescription


  /** Returns a message that is to be displayed to the player at the end of the game. The message
    * will be different depending on whether or not the player has completed their quest. */
  def goodbyeMessage = {
    if (this.isComplete)
      "You start the helicopter and fly away. \nCongratulations, you're safe!" + "\n\nYour score is: " + this.player.experience*200/turnCount
    else if (this.player.isDead)
      "Oh no! You're dead.\nGame over!"
    else  // game over due to player quitting
      "Quitter!"
  }


  /** Plays a turn by executing the given in-game command, such as "go west". Returns a textual
    * report of what happened, or an error message if the command was unknown. In the latter
    * case, no turns elapse. */
  def playTurn(command: String) = {
    val action = new Action(command)
    val outcomeReport = action.execute(this.player)
    if (outcomeReport.isDefined) {
      this.turnCount += 1
      this.player.randomizerNumber += 1
    }
    outcomeReport.getOrElse("Unknown command: \"" + command + "\".")
  }


}

