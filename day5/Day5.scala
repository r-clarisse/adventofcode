object Day5 {

    case class Coordinate(x: Int, y: Int)

    case class Graph() {
        var drawnGraph =  scala.collection.mutable.Map[Coordinate, Int]().withDefault(k => 0)

        def updateGraph(coord: Coordinate): Unit ={
            drawnGraph(coord) += 1
        }
    }

    def drawVerticalLine(a: Coordinate, b: Coordinate): IndexedSeq[Coordinate] = {
        val yFactor = b.y.compareTo(a.y)
        for (i <- a.y to b.y by yFactor) yield Coordinate(a.x, i)
    }

    def drawHorizontalLine(a: Coordinate, b: Coordinate): IndexedSeq[Coordinate] = {
        val xFactor = b.x.compareTo(a.x)
        for (i <- a.x to b.x by xFactor) yield Coordinate(i, a.y)
    }

    def drawDiagonal(a: Coordinate, b: Coordinate): IndexedSeq[Coordinate] = {
        val xFactor = b.x.compareTo(a.x)
        val yFactor = b.y.compareTo(a.y)

        val xCoords = a.x to b.x by xFactor
        val yCoords = a.y to b.y by yFactor

        (xCoords zip yCoords).map(t => Coordinate(t._1, t._2))
    }

    def drawLineWithoutDiagonal(a: Coordinate, b: Coordinate): IndexedSeq[Coordinate] = (a,b) match {
        case (a,b) if a.x == b.x && a.y > b.y => drawVerticalLine(a, b)
        case (a,b) if a.x == b.x && a.y < b.y => drawVerticalLine(a, b)
        case (a,b) if a.y == b.y && a.x > b.x  => drawHorizontalLine(a, b)
        case (a,b) if a.y == b.y && a.x < b.x  => drawHorizontalLine(a, b)
        case (_,_) => IndexedSeq()
    }

    def drawLine(a: Coordinate, b: Coordinate): IndexedSeq[Coordinate] = (a,b) match {
        case (a,b) if a.x == b.x && a.y > b.y => drawVerticalLine(a, b)
        case (a,b) if a.x == b.x && a.y < b.y => drawVerticalLine(a, b)
        case (a,b) if a.y == b.y && a.x > b.x  => drawHorizontalLine(a, b)
        case (a,b) if a.y == b.y && a.x < b.x => drawHorizontalLine(a, b)
        case (a,b) if Math.abs(a.y - b.y) == Math.abs(a.x - b.x) => drawDiagonal(a, b)
        case (_,_) => IndexedSeq()
    }

    def main(args: Array[String]): Unit = {
        val bullshit = io.Source.fromFile(args(0)).getLines().toList.map(_.split(" -> "))

        val pointsPairs = bullshit.map(b => (b(0).split(","), b(1).split(","))).map(a => (Coordinate(a._1(0).toInt, a._1(1).toInt), Coordinate(a._2(0).toInt, a._2(1).toInt)))

        val graph1 = Graph()
        pointsPairs.map(pp => drawLineWithoutDiagonal(pp._1, pp._2)).flatMap(c => c).foreach(graph1.updateGraph)
        

        println(s"part1: ${graph1.drawnGraph.values.count(_ >= 2)}")
        
        val graph2 = Graph()
        pointsPairs.map(pp => drawLine(pp._1, pp._2)).flatMap(c => c).foreach(graph2.updateGraph)

        println(graph2.drawnGraph.values.count(_ >= 2))

    }

}