object Day6 {
    def main(args: Array[String]): Unit = {

        val input = io.Source.fromFile(args(0)).getLines().take(1).mkString.split(",").map(_.toInt).toList
        
        val mapTest = scala.collection.mutable.Map[Int, Long](0 -> 0, 1 -> 0, 2 -> 0, 3 -> 0, 4 -> 0, 5 -> 0, 6 -> 0, 7 -> 0, 8 -> 0)
        input.groupMapReduce(identity)(_ => 1)(_ + _).foreach(e => mapTest(e._1) += e._2)

        for (i <- 1 to args(1).toInt) {
            val tmp0 = mapTest(0);
            for(j <- mapTest.keySet) {
                if (j == 8) {
                    mapTest(j) = tmp0
                } else {
                    mapTest(j) -= mapTest(j) - mapTest(j+1)
                    if (j == 6) mapTest(j) += tmp0
                }
            }
        }
        println(mapTest.values.sum)
    }
}