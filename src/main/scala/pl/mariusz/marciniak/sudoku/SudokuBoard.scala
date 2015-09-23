package pl.mariusz.marciniak.sudoku

object SudokuBoardUtil {
  def size = 9;
  def squareSize = 3;
  val DEBUG = false;
  def log(message: String) = if(DEBUG) println(message)
  def logBoard(board: SudokuBoard) = if(DEBUG) board.foreach(v => println(v mkString("::")))

  type SudokuBoard = Seq[Seq[SudokuCell]]

  def breakTextBoardIntoLines(bText: String): List[String] = bText.split(";").toList

  def breakLineIntoValues(line: String): List[SudokuCell] = line.split(",").toList map (
    sign =>
      if (sign.trim.length > 0)
        new SudokuCell(List(sign.trim.toInt))
      else
        null)

  def parseBoard(bText: String): SudokuBoard = for {
    line <- breakTextBoardIntoLines(bText)
  } yield breakLineIntoValues(line)

  def elementsDefinedInRow(row: Int, board: SudokuBoard): Seq[Int] = for {
    cell <- board(row)
    if (cell != null && cell.isDefined)
  } yield cell.getPossibleValues(0)

  def elementsDefinedInColumn(col: Int, board: SudokuBoard): Seq[Int] = for {
    row <- 0 to size - 1
    if (board(row)(col) != null && board(row)(col).isDefined)
  } yield board(row)(col).getPossibleValues(0)

  def squareFirstRow(row: Int): Int = row / squareSize * squareSize

  def squareFirstColumn(col: Int): Int = col / squareSize * squareSize

  def elementsDefinedInSquare(row: Int, col: Int, board: SudokuBoard): Seq[Int] = {
    val sqR = squareFirstRow(row)
    val sqC = squareFirstColumn(col)
    for {
      r <- sqR to sqR + squareSize - 1
      c <- sqC to sqC + squareSize - 1
      if (board(r)(c) != null && board(r)(c).isDefined)
    } yield board(r)(c).getPossibleValues(0)
  }

  def elementsNotIn(coll: Seq[Int]): Seq[Int] = for {
    v <- 1 to size
    if (!coll.contains(v))
  } yield v

  def boardScore(board: SudokuBoard): Int = board.flatten.count(cell => if (cell != null) cell.isDefined else false)

  def isBoardInvalid(board: SudokuBoard): Boolean = {
    board.flatten.exists(cell => cell != null && cell.noMoreValues)
  }

  def hasDuplicates(collection: Seq[Int]): Boolean = collection.distinct.length != collection.length

  def calculatePossibleValuesForCells(board: SudokuBoard): SudokuBoard = {
    val elementsInRows = for {
      r <- 0 to size - 1
    } yield elementsDefinedInRow(r, board)
    if (elementsInRows.find(row => hasDuplicates(row)) != None) throw new SudokuAlgorithmException("Duplicates in row")
    
    val elementsInColumns = for {
      c <- 0 to size - 1
    } yield elementsDefinedInColumn(c, board)
    if (elementsInColumns.find(column => hasDuplicates(column)) != None) throw new SudokuAlgorithmException("Duplicates in column")

    val elementsInSquare = for {
      r <- 0 to size - 1 by squareSize
      c <- 0 to size - 1 by squareSize
    } yield elementsDefinedInSquare(r, c, board)
    if (elementsInSquare.find(square => hasDuplicates(square)) != None) throw new SudokuAlgorithmException("Duplicates in square")

    def calculateBoard(board: SudokuBoard): SudokuBoard = {
      (0 to size - 1) map (r => calculateRow(r, board))
    }

    def calculateRow(r: Int, board: SudokuBoard): Seq[SudokuCell] = {
      (0 to size - 1) map (c =>
        if (board(r)(c) == null || !board(r)(c).isDefined) {
          new SudokuCell(elementsNotIn(elementsInRows(r) ++ elementsInColumns(c) ++ elementsInSquare((r / squareSize) * (size / squareSize) + c / squareSize)))
        } else
          board(r)(c))
    }
    calculateBoard(board)
  }


  def solveSudoku(board: SudokuBoard): SudokuBoard = {
    log("--------------------------------------------------")
    val score = boardScore(board)
    if (score == size * size) {
      board
    } else {
      val newBoard = calculatePossibleValuesForCells(board)

      if (newBoard == null || isBoardInvalid(newBoard)) 
        throw new SudokuAlgorithmException("Invalid board")
      else {
        logBoard(newBoard)

        if (boardScore(newBoard) == score)
          choosePossibleValue(newBoard)
        else 
          solveSudoku(newBoard)
      }
    }
  }

  def replaceCell(row: Int, column: Int, board: SudokuBoard, newCell: SudokuCell): SudokuBoard = {
    board.updated(row, board(row).updated(column, newCell))
  }

  def choosePossibleValue(board: SudokuBoard): SudokuBoard = {
    val min = (board.flatten.minBy(x=> if(x.isDefined) 99 else x.getPossibleValues.size)).getPossibleValues.size
    def boards = for {
      r <- 0 to size - 1
      c <- 0 to size - 1
      if (board(r)(c).getPossibleValues.length == min)
      v <- board(r)(c).getPossibleValues
    } yield replaceCell(r, c, board, new SudokuCell(List(v)))
    try {
       boards.foldLeft[SudokuBoard](null)((b1,b2) => if(b1==null) solveSudoku(b2) else b1)
    } catch {
      case e :SudokuAlgorithmException => {
        log("Exception:"+e.getMessage)
        null
      }
    }
  }

}