package o1.adventure
import scala.math._
import scala.util.Random

abstract class Enemy(val name: String) {

  var currentHP: Int
  var currentAttack: Vector[Int]

  var experience: Int

  var itemDrop: Option[Item]

  def isDead = currentHP == 0

  def healthRemaining = currentHP

  def takeDamage(attack: Int) = {
   var takenDamage = min(healthRemaining, attack)
   currentHP = max(healthRemaining - attack, 0)
   "You deal " + takenDamage + " damage."
  }

  def hasItem = itemDrop.isDefined



}

class Enemy1(name: String) extends Enemy(name) {
  var currentHP = 6
  var currentAttack = 1.to(2).toVector
  var itemDrop: Option[Item] = None

  var experience = 3


}

class Enemy2(name: String) extends Enemy(name) {
  var currentHP = 9
  var currentAttack = 1.to(3).toVector
  var itemDrop: Option[Item] = None

  var experience = 7

}

class Enemy3(name: String) extends Enemy(name) {
  var currentHP = 12
  var currentAttack = 2.to(4).toVector
  var itemDrop: Option[Item] = None

  var experience = 10

}



class RageWalker(name: String) extends Enemy(name) {
  var currentHP = 25
  var currentAttack = 2.to(4).toVector
  var itemDrop: Option[Item] = Some(new Potion("bandage", "Heals 5 HP", 5))

  var experience = 25
}

class Mordath(name: String) extends Enemy(name) {
  var currentHP = 35
  var currentAttack = 3.to(6).toVector
  var itemDrop: Option[Item] = Some(new Potion("medical kit", "Heals 10 HP", 10))

  var experience = 35
}

class Skinrender(name: String) extends Enemy(name) {
  var currentHP = 40
  var currentAttack = 3.to(6).toVector
  var itemDrop: Option[Item] = Some(new SleepDart("sleep dart", "Hmm... wonder what this could be useful for."))

  var experience = 60
}

class CorpseBreath(name: String) extends Enemy(name) {
  var currentHP = 100
  var currentAttack = 8.to(12).toVector
  var itemDrop: Option[Item] = Some(new Achievement("key", "It's for a helicopter!", 0))

  var experience = 100
}




