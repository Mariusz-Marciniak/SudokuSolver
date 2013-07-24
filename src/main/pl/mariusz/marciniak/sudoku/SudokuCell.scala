package pl.mariusz.marciniak.sudoku

class SudokuCell (possibleValues: Seq[Int]) {

	def isDefined = possibleValues.length==1
	
	def noMoreValues = possibleValues.length==0
	
	def getPossibleValues = possibleValues
	
	override def toString() = possibleValues mkString ","
	
}