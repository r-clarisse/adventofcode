object Day7 {
    def main(args: Array[String]): Unit = {
        val input = io.Source.fromFile(args(0)).getLines().take(1).mkString.split(",").map(_.toInt).toList

        val median = input.sortWith(_ < _).drop(input.length/2).head

        val res1 = input.map(value => Math.abs(value - median)).sum

        val avg = input.sum.toFloat / input.size

        val functionPart1 = (start: Int, end: Int) => Math.abs(start - end)
        val part1 = (input.min to input.max)
            .map(minPossible => input.map(functionPart1(_, minPossible)).sum)
            .min
        
        
        val functionPart2 = (start: Int, end: Int) => (0 to Math.abs(start - end)).sum
        val part2 = (input.min to input.max)
            .map(minPossible => input.map(functionPart2(_, minPossible)).sum)
            .min

        println(s"part 1: ${part1}")
        println(s"part 2: ${part2}")
    }
}