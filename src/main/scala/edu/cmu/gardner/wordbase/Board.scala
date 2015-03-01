package edu.cmu.gardner.wordbase

import scala.collection.mutable
import scalax.io.Resource

class Board(
    val characters: Array[Array[Char]],
    val bombs: Seq[(Int, Int)],
    dictionary: Set[String]) {
  val size = (characters.size, characters(0).size)

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
    val moves = squares.flatMap(getPossibleMoves).toSeq
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
    val new_state = current_state.afterMove(move, player)
    if (player == 0) {
      new_state.active_squares._1.size - 2*new_state.active_squares._2.size
    } else {
      new_state.active_squares._2.size - 2*new_state.active_squares._1.size
    }
    /*
    move.size + 2.5* move.map(_._1).max
   if (player == 0) {
      move.map(_._1).max
   } else {
      -move.map(_._1).min
   }
    */
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

  def main(args: Array[String]) {
    val game = game2()
    val board = game._1
    val state = game._2
    val player = game._3
    val moves = board.getPlayerMoves(state, player, 20)
    board.printBoardState(state)
    for (move <- moves) {
      println(move._2)
      board.printMove(state, move._1, player)
    }
  }

  def game1() = {
    val _chars = Array(
      "carflesrid",
      "luflmsaslo",
      "tmieniryac",
      "ngpateopsk",
      "uitredsoig",
      "hsigzilegn",
      "twgunaosut",
      "ioeltortef",
      "hroyiutygo",
      "ampchgehna",
      "stirestcfn",
      "iutoathgif",
      "mlicspoyea"
      )
    val chars = _chars.map(_.toCharArray)
    val bombs = Seq()
    val dictionary_file = "big_dictionary.txt"
    val dictionary = Resource.fromFile(dictionary_file).lines().map(_.toLowerCase).toSet
    val board = new Board(chars, bombs, dictionary)
    var state = board.current_state
    val player = 1
    val p1_squares = new mutable.HashSet[(Int, Int)]
    p1_squares += Tuple2(1, 9)
    p1_squares += Tuple2(2, 8)
    p1_squares += Tuple2(2, 9)
    p1_squares += Tuple2(3, 8)
    p1_squares += Tuple2(3, 9)
    p1_squares += Tuple2(4, 6)
    p1_squares += Tuple2(4, 7)
    p1_squares += Tuple2(4, 8)
    p1_squares += Tuple2(4, 9)
    p1_squares += Tuple2(5, 5)
    p1_squares += Tuple2(5, 6)
    p1_squares += Tuple2(5, 7)
    p1_squares += Tuple2(5, 8)
    p1_squares += Tuple2(5, 9)
    p1_squares += Tuple2(6, 6)
    val p2_squares = new mutable.HashSet[(Int, Int)]
    p2_squares += Tuple2(11, 4)
    p2_squares += Tuple2(10, 5)
    p2_squares += Tuple2(10, 6)
    p2_squares += Tuple2(9, 6)
    state = new BoardState(
      board,
      (state.active_squares._1 ++ p1_squares, state.active_squares._2 ++ p2_squares),
      state.bombs_exploded)
    board.current_state = state
    (board, state, player)
  }

  def game2() = {
    val _chars = Array(
      "aoictfoslh",
      "bplvlrione",
      "ulygoepwro",
      "niormusaot",
      "snayringio",
      "uelcarelsc",
      "paskputyla",
      "tpoesnkasf",
      "lcrstsawft",
      "roiuerlpma",
      "xhfzatutor",
      "ecrnlenoce",
      "rtoerifadp"
      )
    val chars = _chars.map(_.toCharArray)
    val bombs = Seq((1, 0), (7, 7))
    val dictionary_file = "big_dictionary.txt"
    val dictionary = Resource.fromFile(dictionary_file).lines().map(_.toLowerCase).toSet
    val board = new Board(chars, bombs, dictionary)
    var state = board.current_state
    val player = 0
    val p1_squares = new mutable.HashSet[(Int, Int)]
    p1_squares += Tuple2(1, 7)
    p1_squares += Tuple2(2, 7)
    p1_squares += Tuple2(3, 6)
    p1_squares += Tuple2(4, 5)
    p1_squares += Tuple2(4, 6)
    p1_squares += Tuple2(4, 7)
    val p2_squares = new mutable.HashSet[(Int, Int)]
    p2_squares += Tuple2(11, 4)
    p2_squares += Tuple2(11, 5)
    p2_squares += Tuple2(10, 4)
    p2_squares += Tuple2(10, 5)
    p2_squares += Tuple2(9, 4)
    p2_squares += Tuple2(8, 5)
    state = new BoardState(
      board,
      (state.active_squares._1 ++ p1_squares, state.active_squares._2 ++ p2_squares),
      state.bombs_exploded)
    board.current_state = state
    (board, state, player)
  }

  def old_game1() = {
    val _chars = Array(
      "lusulcsidu",
      "naoenaevup",
      "tcrnvetlro",
      "ceacitytie",
      "udsbsrecnv",
      "asbociszog",
      "graltnthbr",
      "outxeareon",
      "lipslitbws",
      "svdirnblgp",
      "teotcainir",
      "ruyrlwmrct",
      "drstxeksoy"
      )
    val chars = _chars.map(_.toCharArray)
    val bombs = Seq((3, 4), (8, 2))
    val dictionary_file = "big_dictionary.txt"
    val dictionary = Resource.fromFile(dictionary_file).lines().map(_.toLowerCase).toSet
    val board = new Board(chars, bombs, dictionary)
    var state = board.current_state
    val player = 0
    val p1_squares = new mutable.HashSet[(Int, Int)]
    p1_squares += Tuple2(7, 4)
    p1_squares += Tuple2(6, 3)
    p1_squares += Tuple2(6, 2)
    p1_squares += Tuple2(5, 2)
    p1_squares += Tuple2(5, 1)
    p1_squares += Tuple2(4, 0)
    p1_squares += Tuple2(3, 0)
    p1_squares += Tuple2(2, 1)
    p1_squares += Tuple2(1, 0)
    p1_squares += Tuple2(1, 1)
    p1_squares += Tuple2(1, 2)
    val p2_squares = new mutable.HashSet[(Int, Int)]
    p2_squares += Tuple2(11, 3)
    p2_squares += Tuple2(10, 2)
    p2_squares += Tuple2(10, 1)
    p2_squares += Tuple2(9, 2)
    p2_squares += Tuple2(9, 0)
    state = new BoardState(
      board,
      (state.active_squares._1 ++ p1_squares, state.active_squares._2 ++ p2_squares),
      state.bombs_exploded)
    board.current_state = state
    (board, state, player)
  }

  def old_game2() = {
    val _chars = Array(
      "spbimarinu",
      "eulntsbtrs",
      "bxtageiapt",
      "ouorpanola",
      "rtnunstril",
      "isailceosn",
      "relqkeltng",
      "tuaueroiel",
      "irmitspwhw",
      "lstprxmlsa",
      "ucrlealmed",
      "oatcuoisan",
      "gnsicpruqe"
      )
    val chars = _chars.map(_.toCharArray)
    val bombs = Nil
    val dictionary_file = "big_dictionary.txt"
    val dictionary = Resource.fromFile(dictionary_file).lines().map(_.toLowerCase).toSet
    val board = new Board(chars, bombs, dictionary)
    var state = board.current_state
    val player = 0
    val p1_squares = new mutable.HashSet[(Int, Int)]
    p1_squares += Tuple2(1, 3)
    p1_squares += Tuple2(1, 4)
    p1_squares += Tuple2(1, 9)
    p1_squares += Tuple2(2, 5)
    p1_squares += Tuple2(2, 9)
    p1_squares += Tuple2(3, 5)
    p1_squares += Tuple2(3, 8)
    p1_squares += Tuple2(3, 9)
    p1_squares += Tuple2(4, 6)
    p1_squares += Tuple2(4, 8)
    p1_squares += Tuple2(4, 9)
    p1_squares += Tuple2(5, 5)
    p1_squares += Tuple2(5, 6)
    p1_squares += Tuple2(5, 7)
    p1_squares += Tuple2(5, 9)
    p1_squares += Tuple2(6, 5)
    p1_squares += Tuple2(6, 6)
    p1_squares += Tuple2(7, 5)
    p1_squares += Tuple2(7, 6)
    p1_squares += Tuple2(7, 7)
    p1_squares += Tuple2(8, 6)
    val p2_squares = new mutable.HashSet[(Int, Int)]
    p2_squares += Tuple2(11, 4)
    p2_squares += Tuple2(11, 5)
    p2_squares += Tuple2(11, 6)
    p2_squares += Tuple2(11, 7)
    p2_squares += Tuple2(11, 8)
    p2_squares += Tuple2(11, 9)
    p2_squares += Tuple2(10, 3)
    p2_squares += Tuple2(10, 4)
    p2_squares += Tuple2(10, 5)
    p2_squares += Tuple2(10, 6)
    p2_squares += Tuple2(10, 7)
    p2_squares += Tuple2(10, 8)
    p2_squares += Tuple2(10, 9)
    p2_squares += Tuple2(9, 3)
    p2_squares += Tuple2(9, 4)
    p2_squares += Tuple2(9, 5)
    p2_squares += Tuple2(9, 6)
    p2_squares += Tuple2(9, 7)
    p2_squares += Tuple2(9, 9)
    p2_squares += Tuple2(8, 3)
    p2_squares += Tuple2(8, 4)
    p2_squares += Tuple2(8, 5)
    p2_squares += Tuple2(7, 4)
    state = new BoardState(
      board,
      (state.active_squares._1 ++ p1_squares, state.active_squares._2 ++ p2_squares),
      state.bombs_exploded)
    board.current_state = state
    (board, state, player)
  }

  def old_game3() = {
    val _chars = Array(
      "plsactedgr",
      "eomnaikgsa",
      "iksiprlinm",
      "asabiahnue",
      "trelneipet",
      "lhtgldyksr",
      "aysbnwesap",
      "ldasoirnre",
      "ientvlnlil",
      "tociergank",
      "maltshlucu",
      "eseoapinol",
      "rtsmrepaep"
      )
    val chars = _chars.map(_.toCharArray)
    val bombs = Seq((8, 3))
    val dictionary_file = "big_dictionary.txt"
    val dictionary = Resource.fromFile(dictionary_file).lines().map(_.toLowerCase).toSet
    val board = new Board(chars, bombs, dictionary)
    var state = board.current_state
    val player = 0
    val p1_squares = new mutable.HashSet[(Int, Int)]
    p1_squares += Tuple2(1, 5)
    p1_squares += Tuple2(1, 7)
    p1_squares += Tuple2(1, 8)
    p1_squares += Tuple2(1, 9)
    p1_squares += Tuple2(2, 6)
    p1_squares += Tuple2(2, 7)
    p1_squares += Tuple2(2, 8)
    p1_squares += Tuple2(2, 9)
    p1_squares += Tuple2(3, 6)
    p1_squares += Tuple2(3, 9)
    p1_squares += Tuple2(4, 6)
    p1_squares += Tuple2(4, 9)
    val p2_squares = new mutable.HashSet[(Int, Int)]
    p2_squares += Tuple2(11, 7)
    p2_squares += Tuple2(11, 8)
    p2_squares += Tuple2(11, 9)
    p2_squares += Tuple2(10, 8)
    p2_squares += Tuple2(10, 9)
    p2_squares += Tuple2(9, 7)
    p2_squares += Tuple2(9, 8)
    p2_squares += Tuple2(9, 9)
    p2_squares += Tuple2(8, 8)
    p2_squares += Tuple2(8, 9)
    p2_squares += Tuple2(7, 8)
    p2_squares += Tuple2(7, 9)
    p2_squares += Tuple2(6, 8)
    p2_squares += Tuple2(5, 7)
    p2_squares += Tuple2(5, 9)
    p2_squares += Tuple2(4, 8)
    state = new BoardState(
      board,
      (state.active_squares._1 ++ p1_squares, state.active_squares._2 ++ p2_squares),
      state.bombs_exploded)
    board.current_state = state
    (board, state, player)
  }

  def old_game4() = {
    val _chars = Array(
      "mhpuosrotw",
      "ociparimea",
      "rnsoetaudr",
      "edertasped",
      "amsiherhpl",
      "yupropirou",
      "ntsenatnci",
      "eirsutlart",
      "arlfnhecia",
      "tsudeocnrd",
      "neatimeocg",
      "urueseghia",
      "gtionusenm"
      )
    val chars = _chars.map(_.toCharArray)
    val bombs = Nil
    val dictionary_file = "big_dictionary.txt"
    val dictionary = Resource.fromFile(dictionary_file).lines().map(_.toLowerCase).toSet
    val board = new Board(chars, bombs, dictionary)
    var state = board.current_state
    val player = 0
    val p1_squares = new mutable.HashSet[(Int, Int)]
    p1_squares += Tuple2(1, 2)
    p1_squares += Tuple2(1, 3)
    p1_squares += Tuple2(1, 4)
    p1_squares += Tuple2(2, 2)
    p1_squares += Tuple2(2, 3)
    p1_squares += Tuple2(2, 4)
    p1_squares += Tuple2(3, 2)
    p1_squares += Tuple2(3, 3)
    p1_squares += Tuple2(4, 1)
    p1_squares += Tuple2(4, 2)
    p1_squares += Tuple2(4, 3)
    p1_squares += Tuple2(5, 1)
    p1_squares += Tuple2(5, 2)
    p1_squares += Tuple2(5, 3)
    p1_squares += Tuple2(6, 2)
    val p2_squares = new mutable.HashSet[(Int, Int)]
    p2_squares += Tuple2(11, 3)
    p2_squares += Tuple2(11, 4)
    p2_squares += Tuple2(10, 2)
    p2_squares += Tuple2(10, 3)
    p2_squares += Tuple2(10, 4)
    p2_squares += Tuple2(9, 3)
    p2_squares += Tuple2(9, 4)
    p2_squares += Tuple2(8, 3)
    p2_squares += Tuple2(8, 4)
    p2_squares += Tuple2(7, 2)
    p2_squares += Tuple2(7, 3)
    p2_squares += Tuple2(7, 4)
    p2_squares += Tuple2(6, 3)
    state = new BoardState(
      board,
      (state.active_squares._1 ++ p1_squares, state.active_squares._2 ++ p2_squares),
      state.bombs_exploded)
    board.current_state = state
    (board, state, player)
  }

  def old_game5() = {
    val _chars = Array(
      "rmihnoctbw",
      "elunzroiti",
      "ldhygusmot",
      "yoraqapblv",
      "iletrinosi",
      "emlovasetd",
      "uaretnhcdn",
      "rtanhoabut",
      "psegniteop",
      "lcsilcurhc",
      "uebtabtcli",
      "gueocsibua",
      "myrledumns"
      )
    val chars = _chars.map(_.toCharArray)
    val bombs = Seq((0, 10))
    val dictionary_file = "big_dictionary.txt"
    val dictionary = Resource.fromFile(dictionary_file).lines().map(_.toLowerCase).toSet
    val board = new Board(chars, bombs, dictionary)
    var state = board.current_state
    val player = 0
    val p1_squares = new mutable.HashSet[(Int, Int)]
    p1_squares += Tuple2(1, 6)
    p1_squares += Tuple2(2, 4)
    p1_squares += Tuple2(2, 5)
    p1_squares += Tuple2(3, 2)
    p1_squares += Tuple2(3, 3)
    p1_squares += Tuple2(3, 5)
    p1_squares += Tuple2(4, 1)
    p1_squares += Tuple2(4, 2)
    p1_squares += Tuple2(4, 3)
    p1_squares += Tuple2(5, 2)
    p1_squares += Tuple2(5, 3)
    p1_squares += Tuple2(5, 4)
    val p2_squares = new mutable.HashSet[(Int, Int)]
    p2_squares += Tuple2(11, 2)
    p2_squares += Tuple2(11, 3)
    p2_squares += Tuple2(11, 4)
    p2_squares += Tuple2(11, 6)
    p2_squares += Tuple2(10, 4)
    p2_squares += Tuple2(10, 7)
    p2_squares += Tuple2(9, 5)
    p2_squares += Tuple2(9, 7)
    p2_squares += Tuple2(8, 5)
    p2_squares += Tuple2(8, 6)
    p2_squares += Tuple2(8, 7)
    p2_squares += Tuple2(7, 6)
    p2_squares += Tuple2(6, 5)
    p2_squares += Tuple2(5, 5)
    p2_squares += Tuple2(5, 6)
    p2_squares += Tuple2(4, 4)
    p2_squares += Tuple2(4, 5)
    state = new BoardState(
      board,
      (state.active_squares._1 ++ p1_squares, state.active_squares._2 ++ p2_squares),
      state.bombs_exploded)
    board.current_state = state
    (board, state, player)
  }

  def old_game6() = {
    val _chars = Array(
      "oudeguamrc",
      "ugcorbinha",
      "etntlstdeu",
      "nbesamnckm",
      "erloniesil",
      "ibsigkrhta",
      "cetretkayn",
      "enidbscbmd",
      "dieaoghsie",
      "hcuemnerea",
      "nusotioeto",
      "mlbhftsdsl",
      "ceifederaf"
      )
    val chars = _chars.map(_.toCharArray)
    val bombs = Seq((3, 0), (2, 9))
    val dictionary_file = "big_dictionary.txt"
    val dictionary = Resource.fromFile(dictionary_file).lines().map(_.toLowerCase).toSet
    val board = new Board(chars, bombs, dictionary)
    var state = board.current_state
    val player = 0
    val p1_squares = new mutable.HashSet[(Int, Int)]
    p1_squares += Tuple2(1, 3)
    p1_squares += Tuple2(1, 4)
    p1_squares += Tuple2(2, 3)
    p1_squares += Tuple2(2, 4)
    p1_squares += Tuple2(2, 5)
    p1_squares += Tuple2(3, 3)
    p1_squares += Tuple2(3, 4)
    p1_squares += Tuple2(4, 2)
    p1_squares += Tuple2(5, 3)
    p1_squares += Tuple2(6, 1)
    p1_squares += Tuple2(6, 2)
    p1_squares += Tuple2(7, 0)
    p1_squares += Tuple2(7, 1)
    p1_squares += Tuple2(7, 2)
    p1_squares += Tuple2(8, 0)
    val p2_squares = new mutable.HashSet[(Int, Int)]
    p2_squares += Tuple2(11, 8)
    p2_squares += Tuple2(10, 7)
    p2_squares += Tuple2(10, 8)
    p2_squares += Tuple2(10, 9)
    p2_squares += Tuple2(9, 5)
    p2_squares += Tuple2(9, 6)
    p2_squares += Tuple2(9, 8)
    p2_squares += Tuple2(9, 9)
    p2_squares += Tuple2(8, 4)
    p2_squares += Tuple2(8, 8)
    p2_squares += Tuple2(8, 9)
    p2_squares += Tuple2(7, 5)
    p2_squares += Tuple2(7, 9)
    p2_squares += Tuple2(6, 3)
    p2_squares += Tuple2(6, 4)
    state = new BoardState(
      board,
      (state.active_squares._1 ++ p1_squares, state.active_squares._2 ++ p2_squares),
      state.bombs_exploded)
    board.current_state = state
    (board, state, player)
  }
}
