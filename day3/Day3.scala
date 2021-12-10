object Day3 {

    def determineLineMostCommonBite(report: List[Char]): Char = {
        report.groupMapReduce(identity)(_ => 1)(_ + _).toSeq.sortWith((a,b) => if(a._2 == b._2) a._1 < b._1 else a._2 < b._2).last._1
    }

    def determineLineLeastCommonBite(report: List[Char]): Char = {
        report.groupMapReduce(identity)(_ => 1)(_ + _).toSeq.sortWith((a,b) => if(a._2 == b._2) a._1 > b._1 else a._2 > b._2).last._1
    }

    def determineReportsMostCommonBytes(reports: List[List[Char]]): List[Char] = {
        reports.map(determineLineMostCommonBite)
    }

    def determineArrayLeastCommonBite(reports: List[List[Char]]): List[Char] = {
        reports.map(determineLineLeastCommonBite)
    }

    def tobase2(list: List[Char]): Int = {
        list.map("01".indexOf(_)).reduceLeft(_ * 2 + _)
    }

    def main(args: Array[String]): Unit = {
        val reports = io.Source.fromFile(args(0))

        val reportsAsArray = reports.getLines().toList.map(l => l.toList)
        val transposedReports = reportsAsArray.transpose

        // Part 1
        val mostCommonBites = determineReportsMostCommonBytes(transposedReports)
        val leastCommonBites = determineArrayLeastCommonBite(transposedReports)

        val gammaRate = tobase2(mostCommonBites)
        val epsilonRate = tobase2(leastCommonBites)

        println(s"part1: ${gammaRate * epsilonRate}")

        // Part 2
        val oxygenGeneratorRating = determineOxygenGeneratorRating(reportsAsArray)
        val co2ScrubberRating = determineCo2ScrubberRating(reportsAsArray)

        println(s"part2: ${oxygenGeneratorRating * co2ScrubberRating}")
    }

    def determineOxygenGeneratorRating(array: List[List[Char]], index: Int = 0): Int = {
        val transposedArray = array.transpose
        if (array.length == 1) tobase2(array(0)) else determineOxygenGeneratorRating(array.filter(x => x(index) == determineLineMostCommonBite(transposedArray(index))), index+1)
    }

    def determineCo2ScrubberRating(array: List[List[Char]], index: Int = 0): Int = {
        val transposedArray = array.transpose
        if (array.length == 1) tobase2(array(0)) else determineCo2ScrubberRating(array.filter(x => x(index) == determineLineLeastCommonBite(transposedArray(index))), index+1)
    }
}