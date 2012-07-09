package com.squeed.scalapatiens

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

import com.squeed.scalapatiens._

/**
 * This is my first scala program ever. It simulates an unnamed card game whose purpose
 * is to get all cards into one single stack.
 *
 * How to play (IRL):
 * 1. Shuffle your 52 card deck
 * 2. Draw one card and place onto the table
 * 3. Draw another card and place to the right of the first one.
 * 4. If the second card has the same color (kind) or number as the one adjacent to it,
 * 		one may move the second card (and any cards beneath it) onto the card/stack of cards
 * 		beside it. This also applies to the card/stack three cards to the left, e.g if you have
 * 		five cards on the table:
 * 	|CLUB 5|SPADE 8|HEARTS 9|DIAMONDS 3|SPADE 3|
 *
 *  You can either move SPADE 3 onto DIAMONDS 3, or you can move SPADE 3 onto SPADE 8.
 *
 *  Also, consider this case:
 *  |CLUB 5|SPADE 8|HEARTS 3|DIAMONDS 2|SPADE 3|
 *
 *  Here, you can move SPADE 3 onto SPADE 8, and then, without drawing another card, you can
 *  place HEARTS 3 onto stack 2 where you previously placed SPADE 3.
 *
 *  Anyway, this card game is extremely difficult to complete with all cards in a single stack.
 *  The reason I wrote it was to determine the average number of tries to succeed. About 1350 times,
 *  given that my solution has no "best choice" logic whatsoever.
 *
 *  The original program was written in Java and this is my first Scala program ever, so it's bound
 *  to be pretty badly coded from a Scala point of view.
 */
object MainProgram extends App {

  val debug = false

  var tries = 0

  start

  /**
   * Starts a new round of the game, resetting the deck and game table each time.
   * Will be called recursively until the game has succeeded.
   */
  def start: Unit = {

    var tmpdeck = setupDeck(new ArrayBuffer[Card]())
    var deck = new ArrayBuffer[Card]
    var stacks = new ArrayBuffer[Stack]
    deck.appendAll(tmpdeck)

    gameLoop(deck, stacks)

    if (stacks.size > 1) {
      tries += 1
      start
    } else {
      println("Succeeded after " + tries + " tries!")
    }
  }

  def gameLoop(deck: ArrayBuffer[Card], stacks: ArrayBuffer[Stack]): Unit = {
    val card = drawCard(deck)
    stacks.append(new Stack(new ArrayBuffer[Card]))
    stacks.last.addCard(card)
    if (debug) {
      println("Drew card " + card);
      println("Deck has " + deck.size + " cards left")
    }

    // Try to stack cards
    tryToStack(stacks)

    if (deck.size > 0) {
      gameLoop(deck, stacks)
    } else {
      if (debug)
        println("Game over. Number of stacks: " + stacks.size)
    }
  }

  /**
   * Analyze the current stacks on the table. Starting from the card farthest
   * to the right, try to move the card (and cards underneath) to the adjacent card/stack or
   * three cards away.
   */
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
          val threeSlotsAwayCard = stacks.apply(i - 3).getTopCard()
          if (card1.canStackCard(threeSlotsAwayCard)) {
            performStacking(i, i - 3, stacks)
            tryToStack(stacks)
            break
          }
        }
      }
    }
  }

  /**
   * Moves card(s) from one stack onto another, removing the the "from" stack after
   * putting the cards on top of the "to" stack.
   */
  def performStacking(fromIndex: Integer, toIndex: Integer, stacks: ArrayBuffer[Stack]) = {
    val from = stacks.apply(fromIndex);
    val to = stacks.apply(toIndex);

    to.addStack(from);
    from.clear()
    stacks.remove(fromIndex);
  }

  /**
   * Pulls the next (top) card from the deck and returns it.
   */
  def drawCard(deck: ArrayBuffer[Card]): Card = {
    return deck.remove(0)
  }

  /**
   * Creates the deck of cards using a for-statement and
   * a little shuffle method I found on StackOverflow.
   */
  def setupDeck(deck: ArrayBuffer[Card]): Array[Card] = {
    for (i <- 0 until 13) {
      deck + (new Card(i, "SPADES"))
      deck + (new Card(i, "HEARTS"))
      deck + (new Card(i, "CLUBS"))
      deck + (new Card(i, "DIAMONDS"))
    }

    return Shuffler.shuffle(deck.toList.toArray).toArray
  }

}

/**
 * The actual shuffle method is taken from StackOverflow, but I placed it in an object
 * to make it a bit like a Java static "helper/util/whateveryounameyours" class
 */

/**
 * The shuffle method is copied from StackOverflow. I placed it in an object to
 * make it a bit "Java static" like.
 */
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