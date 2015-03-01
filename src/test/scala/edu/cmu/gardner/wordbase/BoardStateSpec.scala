package edu.cmu.gardner.wordbase

import org.scalatest._
import scala.collection.mutable

class BoardStateSpec extends FlatSpecLike with Matchers {
  val chars = Array(
    Array('h', 'e', 'l', 'l'),
    Array('e', 'e', 'l', 'o'),
    Array('l', 'h', 'e', 'l'),
    Array('l', 'o', 'h', 'o'))
  val bombs = Seq((1, 1), (2, 2))
  val dictionary = Set("hello")

  "pruneUnconnectedSquares" should "remove unconnected squares for player 1" in {
    val base = Set((0, 0), (0, 1), (0, 2), (0, 3))
    val unconnected = Set((2, 0), (2, 1), (3, 0), (3, 1))
    BoardState.pruneUnconnectedSquares(0, base ++ unconnected) should be(base)
    BoardState.pruneUnconnectedSquares(0, base ++ unconnected + Tuple2(1, 3)) should be(base + Tuple2(1, 3))
  }

  it should "keep connected squares for player 1" in {
    val base = Set((0, 0), (0, 1), (0, 2), (0, 3))
    val unconnected = Set((2, 0), (2, 1), (3, 0), (3, 1))
    val connected = base ++ unconnected + Tuple2(1, 2)
    BoardState.pruneUnconnectedSquares(0, connected) should be(connected)
  }


  "pruneUnconnectedSquares" should "remove unconnected squares for player 2" in {
    val base = Set((3, 0), (3, 1), (3, 2), (3, 3))
    val unconnected = Set((1, 0), (1, 1), (0, 0), (0, 1))
    BoardState.pruneUnconnectedSquares(1, base ++ unconnected) should be(base)
    BoardState.pruneUnconnectedSquares(1, base ++ unconnected + Tuple2(2, 3)) should be(base + Tuple2(2, 3))
  }

  it should "keep connected squares for player 2" in {
    val base = Set((3, 0), (3, 1), (3, 2), (3, 3))
    val unconnected = Set((1, 0), (1, 1), (0, 0), (0, 1))
    val connected = base ++ unconnected + Tuple2(2, 2)
    BoardState.pruneUnconnectedSquares(1, connected) should be(connected)
  }

  "afterMove" should "add move squares to the active set for player 1" in {
    val board = new Board(chars, bombs, dictionary)
    val move = Seq((0, 0), (1, 0), (2, 0), (3, 0), (3, 1))
    val initial_state = board.current_state
    val new_state = initial_state.afterMove(move, 0)
    new_state.active_squares._1 should be(initial_state.active_squares._1 ++ move)
  }

  "afterMove" should "remove disconnected squares" in {
    val board = new Board(chars, Nil, dictionary)
    val move_1 = Seq((0, 0), (1, 0), (2, 0))
    val initial_state = board.current_state
    var new_state = initial_state.afterMove(move_1, 0)
    val move_2 = Seq((3, 1), (2, 1), (1, 1), (1, 0))
    new_state = new_state.afterMove(move_2, 1)
    new_state.active_squares._1 should be(initial_state.active_squares._1)
    new_state.active_squares._2 should be(initial_state.active_squares._2 ++ move_2)
    val move_3 = Seq((0, 1), (1, 1), (2, 1))
    new_state = new_state.afterMove(move_3, 0)
    new_state.active_squares._1 should be(initial_state.active_squares._1 ++ move_3)
    new_state.active_squares._2 should be(initial_state.active_squares._2)
  }

  it should "explode bombs, but only once" in {
    val board = new Board(chars, Seq((1, 1)), dictionary)
    val move_1 = Seq((0, 0), (1, 1), (2, 2))
    val initial_state = board.current_state
    board.printBoardState(initial_state)
    var new_state = initial_state.afterMove(move_1, 0)
    board.printBoardState(new_state)
    val exploded_squares = Seq((1, 0), (0, 1), (1, 2), (2, 1))
    new_state.active_squares._1 should be(initial_state.active_squares._1 ++ move_1 ++ exploded_squares)
    new_state.active_squares._2 should be(initial_state.active_squares._2)
    val move_2 = Seq((3, 3), (2, 3), (1, 3), (1, 2), (1, 1), (1, 0))
    new_state = new_state.afterMove(move_2, 1)
    board.printBoardState(new_state)
    new_state.active_squares._1 should be(initial_state.active_squares._1)
    new_state.active_squares._2 should be(initial_state.active_squares._2 ++ move_2)
  }
}
