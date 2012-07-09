package com.squeed.scalapatiens

/**
 * Defines a single playing card with a "color" and a "number". Colors should
 * be the following enums: SPADES, CLUBS, HEARTS, DIAMONDS
 */
case class Card(number: Integer, color: String) {

  /**
   * If the two cards are of the same color or same number, return true
   */
  def canStackCard(other: Card): Boolean = {
    (this.number == other.number || this.color == other.color)
  }
}