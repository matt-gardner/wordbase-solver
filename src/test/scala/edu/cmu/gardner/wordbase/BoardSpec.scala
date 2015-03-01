package edu.cmu.gardner.wordbase

import org.scalatest._
import scala.collection.mutable

class BoardSpec extends FlatSpecLike with Matchers {
  val chars = Array(
    Array('h', 'e', 'l', 'l'),
    Array('o', 'e', 'l', 'h'),
    Array('l', 'h', 'e', 'l'),
    Array('l', 'o', 'l', 'o'))
  val bombs = Seq((1, 0), (2, 2))
  val dictionary = Set("hello")

  "getPossibleMoves" should "get the right moves" in {
    val board = new Board(chars, bombs, dictionary)
    val moves = board.getPossibleMoves((2, 1))
    moves.size should be(6)
    val correct_move_set = new mutable.HashSet[Seq[(Int, Int)]]
    correct_move_set += Seq((2, 1), (2, 2), (2, 3), (3, 2), (3, 3))
    correct_move_set += Seq((2, 1), (2, 2), (3, 2), (2, 3), (3, 3))
    correct_move_set += Seq((2, 1), (2, 2), (1, 2), (2, 3), (3, 3))
    correct_move_set += Seq((2, 1), (2, 2), (2, 3), (3, 2), (3, 1))
    correct_move_set += Seq((2, 1), (1, 1), (2, 0), (3, 0), (3, 1))
    correct_move_set += Seq((2, 1), (1, 1), (1, 2), (2, 3), (3, 3))
    moves.toSet should be(correct_move_set.toSet)
  }
}
