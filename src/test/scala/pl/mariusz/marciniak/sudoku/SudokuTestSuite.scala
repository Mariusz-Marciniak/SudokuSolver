package pl.mariusz.marciniak.sudoku

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SudokuTestSuite extends FunSuite {
  val simpleBoard = """ ,8, ,4, ,9,6,5,3;
                       6,4,2,8, , , ,7, ;
                        , , , , , ,8, , ;
                        , ,7, , ,5, ,4,2;
                        , , ,7, ,1, , , ;
                       8,5, ,6, , ,1, , ;
                        , ,6, , , , , , ;
                        ,1, , , ,4,7,3,6;
                       2,7,3,5, ,8, ,1, ;"""

  val mediumBoard = """  1, , , , ,8, , ,9;
                          , ,2, , , , , ,8;
                          ,8, ,5,4,9, , , ;
                          ,4, ,2, , ,9, , ;
                         3, ,9, , , ,2, ,1;
                          , ,1, , ,5, ,4, ;
                          , , ,9,1,2, ,3, ;
                         7, , , , , ,1, , ;
                         2, , ,7, , , , ,6;"""

  val hardBoard = """     , , ,6, , ,1,4,2;
                          ,2,6, , , , , , ;
                         7, , , , ,4, , ,9;
                          , , , , ,6,2, , ;
                          ,6, ,9, ,1, ,5, ;
                          , ,5,3, , , , , ;
                         9, , ,4, , , , ,7;
                          , , , , , ,8,1, ;
                         8,4,2, , ,7, , , ;"""

  val diabloBoard = """   , , ,7, ,4, , ,5;
                          ,2, , ,1, , ,7, ;
                          , , , ,8, , , ,2;
                          ,9, , , ,6,2,5, ;
                         6, , , ,7, , , ,8;
                          ,5,3,2, , , ,1, ;
                         4, , , ,9, , , , ;
                          ,3, , ,6, , ,9, ;
                         2, , ,4, ,7, , , ;"""

  val invalidBoard = """3, , ,6, , ,1,4,2;
                         ,2,6, , , , , , ;
                        7, , , , ,4, , ,9;
                         , , , , ,6,2, , ;
                         ,6, ,9, ,1, ,5, ;
                         , ,5,3, , , , , ;
                        9, , ,4, , , , ,7;
                         , , , , , ,8,1, ;
                        8,4,2, , ,7, , , ;"""
  val cleanBoard = """ , , , , , , , , ;
                          , , , , , , , , ;
                          , , , , , , , , ;
                          , , , , , , , , ;
                          , , , , , , , , ;
                          , , , , , , , , ;
                          , , , , , , , , ;
                          , , , , , , , , ;
                          , , , , , , , , ;"""

  val simpleSolution = """7,8,1,4,2,9,6,5,3;
                          6,4,2,8,5,3,9,7,1;
                          9,3,5,1,7,6,8,2,4;
                          1,6,7,9,8,5,3,4,2;
                          3,2,9,7,4,1,5,6,8;
                          8,5,4,6,3,2,1,9,7;
                          4,9,6,3,1,7,2,8,5;
                          5,1,8,2,9,4,7,3,6;
                          2,7,3,5,6,8,4,1,9;"""

  val mediumSolution = """1,3,4,6,2,8,5,7,9;
                          9,5,2,1,3,7,4,6,8;
                          6,8,7,5,4,9,3,1,2;
                          5,4,6,2,7,1,9,8,3;
                          3,7,9,4,8,6,2,5,1;
                          8,2,1,3,9,5,6,4,7;
                          4,6,8,9,1,2,7,3,5;
                          7,9,5,8,6,3,1,2,4;
                          2,1,3,7,5,4,8,9,6;"""

  val hardSolution = """5,8,9,6,7,3,1,4,2;
                        4,2,6,5,1,9,3,7,8;
                        7,1,3,8,2,4,5,6,9;
                        3,9,4,7,5,6,2,8,1;
                        2,6,8,9,4,1,7,5,3;
                        1,7,5,3,8,2,4,9,6;
                        9,5,1,4,3,8,6,2,7;
                        6,3,7,2,9,5,8,1,4;
                        8,4,2,1,6,7,9,3,5;"""

  val diabloSolution = """9,8,1,7,2,4,3,6,5;
                          3,2,4,6,1,5,8,7,9;
                          7,6,5,9,8,3,1,4,2;
                          1,9,7,8,3,6,2,5,4;
                          6,4,2,5,7,1,9,3,8;
                          8,5,3,2,4,9,7,1,6;
                          4,7,6,3,9,8,5,2,1;
                          5,3,8,1,6,2,4,9,7;
                          2,1,9,4,5,7,6,8,3;"""

  test("is cell defined") {
    val definedCell = new SudokuCell(List(2))
    assert(definedCell.isDefined, "should be defined")
    val notDefinedCell = new SudokuCell(List(2, 3, 4))
    assert(!notDefinedCell.isDefined, "shouldn't be defined")
  }

  test("parse board utils") {
    val lines = SudokuBoardUtil.breakTextBoardIntoLines(simpleBoard)
    assert(lines.length == 9, "dimension of simple board should be 9")
    val firstLine = SudokuBoardUtil.breakLineIntoValues(lines(1))
    assert(firstLine.length == 9, "dimension of line one of the board should be 9")
  }

  test("parse board") {
    val board = SudokuBoardUtil.parseBoard(simpleBoard)
    val definedInRowOne = SudokuBoardUtil.elementsDefinedInRow(1, board)
    val definedInColThree = SudokuBoardUtil.elementsDefinedInColumn(3, board)
    val definedInSquareFourFive = SudokuBoardUtil.elementsDefinedInSquare(4, 5, board)
    assert(definedInRowOne == List(6, 4, 2, 8, 7), "in row number 1 defined elements are 6,4,2,8,7")
    assert(definedInColThree == List(4, 8, 7, 6, 5), "in column number 3 defined elements are 4,8,7,6,5")
    assert(definedInSquareFourFive == List(5, 7, 1, 6), "in square number 4,5 defined elements are 5,7,1,6")
  }

  test("recalculate board") {
    val board = SudokuBoardUtil.parseBoard(simpleBoard)
    val recalculatedBoard = SudokuBoardUtil.calculatePossibleValuesForCells(board)

    assert(recalculatedBoard(0)(0).getPossibleValues == List(1, 7), "for cell 0,0 possible values are 1,7")
    assert(recalculatedBoard(8)(6).getPossibleValues == List(4, 9), "for cell 8,6 possible values are 4,9")
    assert(recalculatedBoard(7)(2).getPossibleValues == List(5, 8, 9), "for cell 7,2 possible value are 5,8,9")
  }

  test("check board score") {
    val board = SudokuBoardUtil.parseBoard(simpleBoard)
    val boardScore = SudokuBoardUtil.boardScore(board)
    assert(boardScore == 34, "there should be 34 defined elements in simpleBoard")
  }

  test("solve simple sudoku") {
    val solvedSimpleBoard = SudokuBoardUtil.solveSudoku(SudokuBoardUtil.parseBoard(simpleBoard))
    assert(solvedSimpleBoard.flatten.mkString(",") == SudokuBoardUtil.parseBoard(simpleSolution).flatten.mkString(","))
  }
  test("solve medium sudoku") {
    val solvedMediumBoard = SudokuBoardUtil.solveSudoku(SudokuBoardUtil.parseBoard(mediumBoard))
    assert(solvedMediumBoard.flatten.mkString(",") == SudokuBoardUtil.parseBoard(mediumSolution).flatten.mkString(","))
  }
  test("solve hard sudoku") {
    val solvedHardBoard = SudokuBoardUtil.solveSudoku(SudokuBoardUtil.parseBoard(hardBoard))
    assert(solvedHardBoard.flatten.mkString(",") == SudokuBoardUtil.parseBoard(hardSolution).flatten.mkString(","))
  }
  test("solve diablo sudoku") {
    val solvedDiabloBoard = SudokuBoardUtil.solveSudoku(SudokuBoardUtil.parseBoard(diabloBoard))
    assert(solvedDiabloBoard.flatten.mkString(",") == SudokuBoardUtil.parseBoard(diabloSolution).flatten.mkString(","))
  }

  test("invalid sudoku") {
    assert(SudokuBoardUtil.solveSudoku(SudokuBoardUtil.parseBoard(invalidBoard)) == null)
  }

  test("solve clean board") {
    SudokuBoardUtil.solveSudoku(SudokuBoardUtil.parseBoard(cleanBoard))
  }
}