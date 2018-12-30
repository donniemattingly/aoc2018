import java.io.File
import java.lang.Math.abs

data class Nanobot(val x: Long, val y: Long, val z: Long, val r: Long)
data class Pos(val x: Long, val y: Long, val z: Long)

val p = "[^-?0-9]+".toRegex()
fun parseBot(input: String): Nanobot {
    val (coordStr, rStr) = input.split(", ")
    val (x, y, z) = coordStr.replace(p, " ")
        .split(" ")
        .map { it.trim() }
        .filter { it.isNotBlank() }
        .map { it.toLong() }

    val r = rStr.replace(p, " ").trim().toLong()
    return Nanobot(x, y, z, r)
}

fun input(fileName: String): Set<Nanobot> {
    return File(fileName).readLines().map { parseBot(it) }.toSet()
}

fun inRange(a: Nanobot, b: Nanobot): Boolean {
    return a.r >= abs(b.x - a.x) + abs(b.y - a.y) + abs(b.z - a.z)
}

fun intersects(a: Nanobot, b: Nanobot): Boolean {
    return (a.r + b.r) >= abs(b.x - a.x) + abs(b.y - a.y) + abs(b.z - a.z)
}

fun botsInRange(nanobot: Nanobot, nanobots: Set<Nanobot>): Int {
    return nanobots.filter { inRange(nanobot, it) }.count()
}

fun startingRadius(nanobots: Set<Nanobot>): Long {
    val minX = nanobots.minBy { it.x }!!.x
    val minY = nanobots.minBy { it.y }!!.y
    val minZ = nanobots.minBy { it.z }!!.z
    val maxX = nanobots.maxBy { it.x }!!.x
    val maxY = nanobots.maxBy { it.y }!!.y
    val maxZ = nanobots.maxBy { it.z }!!.z

    return abs((maxX - minX) * (maxY - minY) * (maxZ - minZ))
}

fun partOne() {
    val test_file = "../../inputs/input-23.0.txt"
    val real_file = "../../inputs/input-23.1.txt"

    val bots = input(real_file)
    val botWithMostRange = bots.maxBy { it.r }
    val botsInRange = botsInRange(botWithMostRange!!, bots)

    println("part 1: $botsInRange")
}

fun getOptions(pos: Pos, radius: Long): List<Pos> {
    return (-1L..1L).flatMap { itx ->
        val dx = itx * radius
        (-1L..1L).flatMap { ity ->
            val dy = ity * radius
            (-1L..1L).map { itz ->
                val dz = itz * radius
                Pos(pos.x + dx, pos.y + dy, pos.z + dz)
            }
        }
    }
}


// lol it works
fun simpleAscent(nanobots: Set<Nanobot>) {
    var radius = startingRadius(nanobots)
    var options: List<Pos>
    var inRangeCount = 0
    var best: Pos = Pos(0, 0, 0)
    while (radius >= 1) {
        radius /= 2
        options = getOptions(best, radius)

        val result = options.map { it ->
            val option = Nanobot(it.x, it.y, it.z, radius)
            val c = nanobots.filter { intersects(it, option) }.count()

            it to c
        }.maxBy { it.second }!!

        best = result.first
        println(result to radius)
    }

    println("part 2: ${best.x + best.y + best.z}")
}

fun partTwo() {
    val test_file = "../../inputs/input-23.0.txt"
    val real_file = "../../inputs/input-23.1.txt"
    val bots = input(real_file)
    simpleAscent(bots)
}

fun main(args: Array<String>) {
//    partOne()
    partTwo()
}
