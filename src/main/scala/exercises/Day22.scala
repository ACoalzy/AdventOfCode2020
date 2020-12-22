package exercises

import util.DayN

import scala.collection.immutable.Queue

object Day22 extends DayN {
  override val num = 22

  case class Player(id: Int, deck: Queue[Int]) {
    def updated(newDeck: Queue[Int]): Player = Player(id, newDeck)
  }

  def parse(lines: List[String]): (Player, Player) = {
    val decks = lines.mkString("\n").split("\n\n")
      .map(_.split("\n"))
      .map(a => Queue.from(a.drop(1).map(_.toInt)))

    (Player(1, decks.head), Player(2, decks(1)))
  }

  def play(player1: Player, player2: Player): Player = {
    if (player1.deck.isEmpty) player2
    else if (player2.deck.isEmpty) player1
    else {
      val (p1Card, p1Remaining) = player1.deck.dequeue
      val (p2Card, p2Remaining) = player2.deck.dequeue
      if (p1Card > p2Card) play(player1.updated(p1Remaining.enqueueAll(List(p1Card, p2Card))), player2.updated(p2Remaining))
      else play(player1.updated(p1Remaining), player2.updated(p2Remaining.enqueueAll(List(p2Card, p1Card))))
    }
  }

  def recursivePlay(player1: Player, player2: Player): Player = {
    def loop(player1: Player, player2: Player, acc: Set[(Player, Player)]): Player = {
      if (player1.deck.isEmpty) player2
      else if (player2.deck.isEmpty) player1
      else if (acc.contains((player1, player2))) player1
      else {
        val (p1Card, p1Remaining) = player1.deck.dequeue
        val (p2Card, p2Remaining) = player2.deck.dequeue
        val winner = {
          if (p1Remaining.size >= p1Card && p2Remaining.size >= p2Card) {
            loop(player1.updated(p1Remaining.take(p1Card)), player2.updated(p2Remaining.take(p2Card)), Set()).id
          } else if (p1Card > p2Card) player1.id
          else player2.id
        }

        if (winner == player1.id)
          loop(player1.updated(p1Remaining.enqueueAll(List(p1Card, p2Card))), player2.updated(p2Remaining), acc + ((player1, player2)))
        else
          loop(player1.updated(p1Remaining), player2.updated(p2Remaining.enqueueAll(List(p2Card, p1Card))), acc + ((player1, player2)))
      }
    }

    loop(player1, player2, Set())
  }

  def scoreDeck(deck: Queue[Int]): Int = deck.toList.reverse.zipWithIndex.map { case (c, i) => c * (i + 1) }.sum

  val (player1, player2) = parse(lines)
  part1(scoreDeck(play(player1, player2).deck))
  part2(scoreDeck(recursivePlay(player1, player2).deck))
}
