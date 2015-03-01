package edu.cmu.gardner.wordbase

import scala.collection.mutable

class BoardState(
    board: Board,
    val active_squares: (Set[(Int, Int)], Set[(Int, Int)]),
    val bombs_exploded: Seq[Boolean]) {

  def afterMove(move: Seq[(Int, Int)], player: Int): BoardState = {
    var current_player_squares = new mutable.HashSet[(Int, Int)]
    var other_player_squares = new mutable.HashSet[(Int, Int)]
    if (player == 0) {
      current_player_squares ++= active_squares._1
      other_player_squares ++= active_squares._2
    } else {
      current_player_squares ++= active_squares._2
      other_player_squares ++= active_squares._1
    }
    current_player_squares ++= move
    other_player_squares --= move

    val bombs = new Array[Boolean](bombs_exploded.size)
    bombs_exploded.copyToArray(bombs)
    for (i <- 0 until board.bombs.size) {
      val bomb = board.bombs(i)
      if (move.contains(bomb) && !bombs(i)) {
        bombs(i) = true
        if (bomb._1 > 0) {
          current_player_squares += Tuple2(bomb._1 - 1, bomb._2)
          other_player_squares -= Tuple2(bomb._1 - 1, bomb._2)
        }
        if (bomb._2 > 0) {
          current_player_squares += Tuple2(bomb._1, bomb._2 - 1)
          other_player_squares -= Tuple2(bomb._1, bomb._2 - 1)
        }
        if (bomb._1 < board.size._1 - 1) {
          current_player_squares += Tuple2(bomb._1 + 1, bomb._2)
          other_player_squares -= Tuple2(bomb._1 + 1, bomb._2)
        }
        if (bomb._2 < board.size._2 - 1) {
          current_player_squares += Tuple2(bomb._1, bomb._2 + 1)
          other_player_squares -= Tuple2(bomb._1, bomb._2 + 1)
        }
      }
    }

    var p1_squares: Set[(Int, Int)] = null
    var p2_squares: Set[(Int, Int)] = null
    if (player == 0) {
      p1_squares = current_player_squares.toSet
      p2_squares = BoardState.pruneUnconnectedSquares(1, other_player_squares.toSet)
    } else {
      p1_squares = BoardState.pruneUnconnectedSquares(0, other_player_squares.toSet)
      p2_squares = current_player_squares.toSet
    }

    new BoardState(board, (p1_squares, p2_squares), bombs)
  }
}

object BoardState {
  def initialState(board: Board, size: (Int, Int), bombs: Seq[(Int, Int)]) = {
    val active_squares = {
      val player1_squares = new mutable.HashSet[(Int, Int)]
      val player2_squares = new mutable.HashSet[(Int, Int)]
      for (i <- 0 until size._2) {
        player1_squares += Tuple2(0, i)
        player2_squares += Tuple2(size._1 - 1, i)
      }
      (player1_squares.toSet, player2_squares.toSet)
    }
    val bombs_exploded = Array.fill[Boolean](bombs.size)(false)
    new BoardState(board, active_squares, bombs_exploded)
  }

  def pruneUnconnectedSquares(player: Int, _squares: Set[(Int, Int)]): Set[(Int, Int)] = {
    val to_remove = new mutable.HashSet[(Int, Int)]
    var squares = _squares
    if (player == 1) {
      squares = squares.map(pos => (-pos._1, pos._2))
    }

    val starting_row = -10000
    val squares_map = squares.groupBy(_._1)
    val rows = squares_map.keys.toList.sorted
    var current_row = starting_row
    var on_first_row = true
    val active_indices = new mutable.HashSet[Int]
    val prev_active_indices = new mutable.HashSet[Int]
    for (row <- rows) {
      if (current_row != starting_row && row != current_row + 1) {
        prev_active_indices.clear
      }
      val indices = squares_map(row).map(_._2)
      if (current_row == starting_row) {
        active_indices ++= indices
      } else {
        var changed = true
        val unconnected_indices = new mutable.HashSet[Int]
        val connected_indices = new mutable.HashSet[Int]
        unconnected_indices ++= indices
        while (changed) {
          changed = false
          for (index <- unconnected_indices) {
            if (prev_active_indices.contains(index) ||
                prev_active_indices.contains(index - 1) ||
                prev_active_indices.contains(index + 1)) {
              connected_indices += index
              unconnected_indices -= index
              changed = true
            } else if (connected_indices.contains(index - 1) ||
                connected_indices.contains(index + 1)) {
              connected_indices += index
              unconnected_indices -= index
              changed = true
            }
          }
        }
        active_indices ++= connected_indices
        to_remove ++= unconnected_indices.map(index => (row, index))
      }
      prev_active_indices.clear
      prev_active_indices ++= active_indices
      active_indices.clear
      current_row = row
    }

    var result = squares -- to_remove
    if (player == 1) {
      result = result.map(pos => (-pos._1, pos._2))
    }
    result
  }
}
