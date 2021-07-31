package o1.adventure


abstract class Item(val name: String, val description: String) {


  def useCase: String
  var value: Int
  /** Returns a short textual representation of the item (its name, that is). */
  override def toString = this.name


}

class Potion(name: String, description: String, var value: Int) extends Item(name, description) {
  def useCase = "healing"
}

class Achievement(name: String, description: String, var value: Int) extends Item(name, description) {
  def useCase = "achievement"
}

class SleepDart(name: String, description: String) extends Item(name, description) {
  def useCase = "sleepy"
  var value = 2
}