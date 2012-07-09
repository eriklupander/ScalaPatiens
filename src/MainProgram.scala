import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

object MainProgram extends App {
  
  var tries = 0

  println("Starting!")
  var tmpdeck = setupDeck(new ArrayBuffer[Card]())
  var deck = new ArrayBuffer[Card]
  var stacks = new ArrayBuffer[Stack]
  deck.appendAll(tmpdeck)

  gameLoop(deck, stacks)
println("Exit")
  if (stacks.size > 1) {
    tries += 1
    tmpdeck = setupDeck(new ArrayBuffer[Card]())
    deck = new ArrayBuffer[Card]
    stacks = new ArrayBuffer[Stack]
    deck.appendAll(tmpdeck)
    gameLoop(deck, stacks)
  } else {
    println("Succeeded after " + tries + " tries!")
  }

  def gameLoop(deck: ArrayBuffer[Card], stacks: ArrayBuffer[Stack]): Unit = {
    val card = drawCard(deck)
    stacks.append(new Stack(new ArrayBuffer[Card]))
    stacks.last.addCard(card)
    //println("Drew card " + card)
    //println("Deck has " + deck.size + " cards left")

    // Try to stack cards
    tryToStack(stacks)

    if (deck.size > 0) {
      gameLoop(deck, stacks)
    } else {
      println("Game over. Number of stacks: " + stacks.size)
    }
  }

  def tryToStack(stacks: ArrayBuffer[Stack]): Unit = {
    breakable {
      for (i <- (stacks.size - 1) to 1 by -1) {

        val card1 = stacks.apply(i).getTopCard()
        if (i - 1 >= 0) {
          val adjacentCard = stacks.apply(i - 1).getTopCard()
          if (card1.canStackCard(adjacentCard)) {
            performStacking(i, i - 1, stacks)
            tryToStack(stacks)
            break
          }
        }

        if (i - 3 >= 0) {
          val adjacentCard = stacks.apply(i - 3).getTopCard()
          if (card1.canStackCard(adjacentCard)) {
            performStacking(i, i - 3, stacks)
            tryToStack(stacks)
            break
          }
        }
      }
    }
  }

  def performStacking(fromIndex: Integer, toIndex: Integer, stacks: ArrayBuffer[Stack]) = {
    val from = stacks.apply(fromIndex);
    val to = stacks.apply(toIndex);

    to.addStack(from);
    from.clear()
    stacks.remove(fromIndex);
  }

  def drawCard(deck: ArrayBuffer[Card]): Card = {
    return deck.remove(0)
  }

  def setupDeck(deck: ArrayBuffer[Card]): Array[Card] = {
    for (i <- 0 until 13) {
      deck + (new Card(i, "SPADES"))
      deck + (new Card(i, "HEARTS"))
      deck + (new Card(i, "CLUBS"))
      deck + (new Card(i, "DIAMONDS"))
    }

    println("Deck now has " + deck.size + " cards")

    return Shuffler.shuffle(deck.toList.toArray).toArray
  }

}

object Shuffler {
  def shuffle[T](array: Array[T]): Array[T] = {
    val rnd = new java.util.Random
    for (n <- Iterator.range(array.length - 1, 0, -1)) {
      val k = rnd.nextInt(n + 1)
      val t = array(k); array(k) = array(n); array(n) = t
    }
    return array
  }
}

case class Card(number: Integer, color: String) {

  def canStackCard(other: Card): Boolean = {
    if (this.number == other.number) {
      true
    } else if (this.color == other.color) {
      true
    } else {
      false
    }
  }
}

case class Stack(cards: ArrayBuffer[Card]) {
  def getTopCard(): Card = {
    this.cards.apply(0)
  }

  def clear() = {
    this.cards.clear();
  }

  def addCard(card: Card) = {
    this.cards.insert(0, card)
  }

  def addStack(stack: Stack) = {
    this.cards.insertAll(0, stack.cards)
  }

}
