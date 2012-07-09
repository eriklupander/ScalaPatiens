package com.squeed.scalapatiens
import scala.collection.mutable.ArrayBuffer

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