object Day4 {

    case class BingoResult(rollNumber: Int, finalScore: Int)

    case class BingoCell(value: Int, x: Int, y: Int, var marked: Boolean = false)

    case class BingoBoard(board: List[BingoCell]){
        def bingo(): Boolean = {
            board.groupMapReduce(_.x)(_.marked)(_ && _).values.exists(_ == true) || board.groupMapReduce(_.y)(_.marked)(_ && _).values.exists(_ == true)
        }

        def markCell(drawnValue: Int): Option[Int] = {
            val cellOption = board.find(_.value == drawnValue)
            cellOption match{
                case Some(cell) => cell.marked = true
                case None => 
            }

            if(bingo()) Some(computeFinalSCore(drawnValue)) else None
        }

        def computeFinalSCore(lastValue: Int): Int = {
            board.filterNot(_.marked).map(_.value).foldLeft(0)(_ + _) * lastValue
        }
    }

    def main(args: Array[String]): Unit = {
        val numbers = io.Source.fromFile(args(0)).getLines().take(1).mkString.split(",").map(_.toInt)

        val boards = io.Source.fromFile(args(1)).getLines()
            .filterNot(_.isEmpty)
            .grouped(5)
            .toList
            .map(_.map(_.trim).map(_.split(' ').filterNot(_.isEmpty).map(_.toInt).zipWithIndex).toList.zipWithIndex).toList
            .map(_.flatMap({ case(array, y) => array.map({ case(value, x) => (value, x, y)}).toList}))
            .map(_.map(tuple => BingoCell(tuple._1, tuple._2, tuple._3)))
            .map(BingoBoard(_))

        val xd = for(board <- boards) yield for((num, index) <- numbers.zipWithIndex if board.bingo() == false) yield board.markCell(num).map(BingoResult(index, _))
        val listOfResult = xd.flatMap(s => s).filter(_.isDefined)

        val winningBoard = listOfResult.minBy(_.map(_.rollNumber))
        val losingBoard = listOfResult.maxBy(_.filterNot(_.finalScore == 0).map(_.rollNumber))

        println(winningBoard)
        println(losingBoard)
    }
}