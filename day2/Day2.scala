object Day2 {

    abstract class Movement(var horizontalPos:Int = 0, var depth:Int = 0){
        def forward(x: Int): Unit
        def up(x: Int): Unit
        def down(x: Int): Unit

        def gps(instruction: String, distance: Int): Unit = instruction match {
            case "forward" => forward(distance)
            case "up" => up(distance)
            case "down" => down(distance)
        }

        def calculerPosition(): Int = {
            horizontalPos*depth
        }
    }

    case class Position() extends Movement{
        
        override def forward(x: Int): Unit = {
            horizontalPos += x
        }

        override def up(x: Int): Unit = {
            depth -= x
        }

        override def down(x: Int): Unit = {
            depth += x
        }
    }

    case class PositionWithAim(var aim:Int = 0) extends Movement {
        
        override def forward(x: Int): Unit = {
            horizontalPos += x
            depth += (aim * x)
        }

        override def up(x: Int): Unit = {
            aim -= x
        }

        override def down(x: Int): Unit = {
            aim += x
        }
    }


    def main(args: Array[String]): Unit = {
        val directions = io.Source.fromFile("input2.txt")
        val directionsSplitted = directions.getLines().toList.map(s => s.split(" "))


        val positionFinale = Position()
        directionsSplitted.foreach(direction => positionFinale.gps(direction(0), direction(1).toInt))
        println(positionFinale.calculerPosition())


        val positionFinale2 = PositionWithAim()
        directionsSplitted.foreach(direction => positionFinale2.gps(direction(0), direction(1).toInt))
        println(positionFinale2.calculerPosition())
    }
}