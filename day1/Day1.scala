object Day1 {
    def altitudeSuperieure(a:Int, b:Int): Int = (a,b) match {
        case (a,b) if a<b => 1
        case (_,_) => 0
    }
    def main(args: Array[String]): Unit = {
        val lines = io.Source.fromFile("input1.txt").getLines().toList.map(_.toInt)
        println((lines zip lines.tail).map({case (a,b) => altitudeSuperieure(a,b)}).sum)

        val groupOf3 = lines.sliding(3).map(l => l.foldLeft(0)(_+_)).toList
        println((groupOf3 zip groupOf3.tail).map({case (a,b) => altitudeSuperieure(a,b)}).sum)
    }
}