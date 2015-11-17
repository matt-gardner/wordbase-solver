package edu.cmu.gardner.wordbase

import scala.collection.mutable
import scalax.io.Resource

class Board(
    val characters: Array[Array[Char]],
    val bombs: Seq[(Int, Int)],
    dictionary: Set[String]) {
  val size = (characters.size, characters(0).size)

  val MOVE_MULTIPLIER = Board.MOVE_MULTIPLIER
  val DEPTH_MULTIPLIER = Board.DEPTH_MULTIPLIER
  var current_state = BoardState.initialState(this, size, bombs)

  val prefixes = dictionary.par.flatMap(word => {
      (1 to word.length).map(word.substring(0, _))
    }).toSet.seq

  lazy val possible_moves: Map[(Int, Int), Set[Seq[(Int, Int)]]] = {
    val board_positions = for (i <- 0 until size._1; j <- 0 until size._2) yield (i, j)
    board_positions.par.map(x => (x, _getWordsFromPosition(x))).seq.toMap
  }

  def getPossibleMoves(pos: (Int, Int)) = possible_moves(pos)

  def getPlayerMoves(state: BoardState, player: Int, maxMoves: Int): Seq[(Seq[(Int, Int)], String)] = {
    val squares = if (player == 0) state.active_squares._1 else state.active_squares._2
    val moves = squares.flatMap(getPossibleMoves).filter(move =>
        !state.words_played.contains(getWordForMove(move))).toSeq
    moves.sortBy(x => -scoreMove(state, x, player)).take(maxMoves).map(x => (x, getWordForMove(x)))
  }

  def _getWordsFromPosition(pos: (Int, Int)): Set[Seq[(Int, Int)]] = {
    val tmp = new mutable.HashSet[Seq[(Int, Int)]]
    recursiveGetWordsFromPosition(pos, "", Nil, tmp)
    tmp.toSet
  }

  def recursiveGetWordsFromPosition(
      pos: (Int, Int),
      wordSoFar: String,
      seqSoFar: Seq[(Int, Int)],
      words: mutable.HashSet[Seq[(Int, Int)]]): Unit = {
    // First check to see if we should be done - we're at an invalid position, or not a word
    if (!isValidPosition(pos, seqSoFar)) return
    val currentWord = wordSoFar + characters(pos._1)(pos._2)
    if (!prefixes.contains(currentWord)) return  // prefixes is a superset of dictionary

    // Now, check to see if we found a word
    val currentSeq = seqSoFar :+ pos
    if (dictionary.contains(currentWord)) words += currentSeq

    // Finally, recurse
    recursiveGetWordsFromPosition((pos._1 + 1, pos._2 + 1), currentWord, currentSeq, words)
    recursiveGetWordsFromPosition((pos._1 + 1, pos._2 - 1), currentWord, currentSeq, words)
    recursiveGetWordsFromPosition((pos._1 + 1, pos._2), currentWord, currentSeq, words)
    recursiveGetWordsFromPosition((pos._1 - 1, pos._2 + 1), currentWord, currentSeq, words)
    recursiveGetWordsFromPosition((pos._1 - 1, pos._2 - 1), currentWord, currentSeq, words)
    recursiveGetWordsFromPosition((pos._1 - 1, pos._2), currentWord, currentSeq, words)
    recursiveGetWordsFromPosition((pos._1, pos._2 + 1), currentWord, currentSeq, words)
    recursiveGetWordsFromPosition((pos._1, pos._2 - 1), currentWord, currentSeq, words)
  }

  def isValidPosition(pos: (Int, Int), seqSoFar: Seq[(Int, Int)]): Boolean = {
    if (seqSoFar.contains(pos)) return false
    return (pos._1 >= 0 && pos._2 >= 0 && pos._1 < size._1 && pos._2 < size._2)
  }

  def getWordForMove(move: Seq[(Int, Int)]): String = {
    move.map(x => characters(x._1)(x._2)).mkString
  }

  def scoreMove(state: BoardState, move: Seq[(Int, Int)], player: Int) = {
    var score = 0.0
    val new_state = current_state.afterMove(move, player)
    if (player == 0) {
      score += new_state.active_squares._1.size - MOVE_MULTIPLIER * new_state.active_squares._2.size
    } else {
      score += new_state.active_squares._2.size - MOVE_MULTIPLIER * new_state.active_squares._1.size
    }
    if (player == 0 && move.map(_._1).max == 12) {
      score += 1000
    } else if (player == 1 && move.map(_._1).min == 0) {
      score += 1000
    }
    if (player == 0) {
      score += DEPTH_MULTIPLIER * move.map(_._1).max
    } else {
      score += DEPTH_MULTIPLIER * (13 - move.map(_._1).min)
    }
    score
  }

  def printBoardState(state: BoardState) {
    println()
    print(" ")
    for (j <- 0 until size._2) {
      print("--")
    }
    println()
    for (i <- 0 until size._1) {
      print("|")
      for (j <- 0 until size._2) {
        print(" ")
        setColor(state, (i, j))
        print(characters(i)(j))
        resetColor()
      }
      print("|")
      println()
    }
    print(" ")
    for (j <- 0 until size._2) {
      print("--")
    }
    println()
  }

  def setColor(state: BoardState, pos: (Int, Int)) {
    if (bombs.contains(pos)) {
      val index = bombs.indexOf(pos)
      if (index != -1 && !state.bombs_exploded(index)) {
        print(Console.WHITE_B)
        print(Console.BLACK)
      }
    }
    if (state.active_squares._1.contains(pos)) {
      print(Console.YELLOW_B)
      print(Console.BLACK)
    } else if (state.active_squares._2.contains(pos)) {
      print(Console.CYAN_B)
      print(Console.BLACK)
    }
  }

  def resetColor() {
    print(Console.BLACK_B)
    print(Console.WHITE)
  }

  def printMove(state: BoardState, move: Seq[(Int, Int)], player: Int) {
    val separation = 10
    val positions = move.toSet
    val new_state = state.afterMove(move, player)

    // Move
    print(" ")
    for (j <- 0 until size._2) {
      print("--")
    }
    print(" ")

    // Separation
    for (j <- 0 until separation) {
      print(" ")
    }

    // New state
    print(" ")
    for (j <- 0 until size._2) {
      print("--")
    }
    println()

    for (i <- 0 until size._1) {
      // Move
      print("|")
      for (j <- 0 until size._2) {
        print(" ")
        setColor(state, (i, j))
        if (positions.contains((i, j))) {
          print(characters(i)(j))
        } else {
          print(" ")
        }
        resetColor()
      }
      print("|")

      // Separation
      for (j <- 0 until separation) {
        print(" ")
      }

      // New state
      print("|")
      for (j <- 0 until size._2) {
        print(" ")
        setColor(new_state, (i, j))
        print(characters(i)(j))
        resetColor()
      }
      print("|")
      println()
    }

    // Move
    print(" ")
    for (j <- 0 until size._2) {
      print("--")
    }
    print(" ")

    // Separation
    for (j <- 0 until separation) {
      print(" ")
    }

    // New state
    print(" ")
    for (j <- 0 until size._2) {
      print("--")
    }
    println()
    println()
  }
}

object Board {

  val MOVE_MULTIPLIER = 3
  val DEPTH_MULTIPLIER = 0

  def main(args: Array[String]) {
    val game = game1()
    val board = game._1
    val state = game._2
    val player = game._3
    val moves = board.getPlayerMoves(state, player, 20)
    board.printBoardState(state)
    for (move <- moves.reverse) {
      println(move._2)
      board.printMove(state, move._1, player)
    }
  }

  def game1() = {
    val _chars = Array(
      "bhivbpagme",
      "ltslesinid",
      "niluselztu",
      "zgacpiudza",
      "tirphlrsrh",
      "sdberaorce",
      "ledoenpera",
      "iymesighit",
      "sacrzselsd",
      "ureokrpumi",
      "onyigaoylp",
      "daonatnsah",
      "gbscedigre"
      )
    val chars = _chars.map(_.toCharArray)
    val bombs = Seq((6,0), (6, 9))
    val dictionary_file = "big_dictionary.txt"
    val dictionary = Resource.fromFile(dictionary_file).lines().map(_.toLowerCase).toSet
    val board = new Board(chars, bombs, dictionary)
    var state = board.current_state
    val player = 1
    val words_played = Set[String]("blessing", "congressing", "screenies", "resigner", "presser",
      "gored", "proser", "nieces", "edemas", "cremorne", "decrying", "congressed", "bosser",
      "realmless", "changers", "gressing", "hangers", "grapes", "bossier", "lessen", "angles",
      "arsino", "pressing", "gorsier", "presignal")
    val p1_squares = new mutable.HashSet[(Int, Int)]
    p1_squares += Tuple2(1, 3)
    p1_squares += Tuple2(1, 4)
    p1_squares += Tuple2(1, 5)
    p1_squares += Tuple2(1, 6)
    p1_squares += Tuple2(1, 7)
    p1_squares += Tuple2(2, 4)
    p1_squares += Tuple2(3, 3)
    p1_squares += Tuple2(4, 2)
    p1_squares += Tuple2(4, 3)
    p1_squares += Tuple2(4, 4)
    p1_squares += Tuple2(4, 5)
    p1_squares += Tuple2(5, 2)
    p1_squares += Tuple2(5, 3)
    p1_squares += Tuple2(5, 4)
    p1_squares += Tuple2(5, 5)
    p1_squares += Tuple2(6, 3)
    p1_squares += Tuple2(6, 4)
    p1_squares += Tuple2(6, 5)
    p1_squares += Tuple2(7, 4)
    p1_squares += Tuple2(7, 5)
    p1_squares += Tuple2(7, 6)
    val p2_squares = new mutable.HashSet[(Int, Int)]
    p2_squares += Tuple2(11, 2)
    p2_squares += Tuple2(11, 3)
    p2_squares += Tuple2(11, 8)
    p2_squares += Tuple2(10, 4)
    p2_squares += Tuple2(10, 5)
    p2_squares += Tuple2(10, 8)
    p2_squares += Tuple2(9, 3)
    p2_squares += Tuple2(9, 5)
    p2_squares += Tuple2(9, 6)
    p2_squares += Tuple2(9, 8)
    p2_squares += Tuple2(8, 3)
    state = new BoardState(
      board,
      (state.active_squares._1 ++ p1_squares, state.active_squares._2 ++ p2_squares),
      state.bombs_exploded,
      words_played)
    board.current_state = state
    (board, state, player)
  }
}
