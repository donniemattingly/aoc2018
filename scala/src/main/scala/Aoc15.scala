import Terrain.Terrain

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Terrain extends Enumeration {
  type Terrain = Value
  val WALL, OPEN, UNIT = Value
}

class Position(var x: Int, var y: Int) {

  def canEqual(other: Any): Boolean = other.isInstanceOf[Position]

  override def equals(other: Any): Boolean = other match {
    case that: Position =>
      (that canEqual this) &&
        x == that.x &&
        y == that.y
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(x, y)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString = s"Pos($x, $y)"
}

class Cell(var pos: Position, var terrain: Terrain, val symbol: String) {
  var parents: ListBuffer[Cell] = new ListBuffer()
  var level: Int = -1

  def addParent(parent: Cell): Unit = {
    if (parent.level != this.level) {
      this.parents += parent
      this.level = parent.level + 1
    }
  }


  def adjacent(otherCell: Cell): Boolean = {
    if(this.pos.x == otherCell.pos.x){
      Math.abs(this.pos.y - otherCell.pos.y) == 1
    } else if(this.pos.y == otherCell.pos.y){
      Math.abs(this.pos.x - otherCell.pos.x) == 1
    } else {
      false
    }
  }

  override def toString = s"Cell($pos, $terrain)"

  def canEqual(other: Any): Boolean = other.isInstanceOf[Cell]

  override def equals(other: Any): Boolean = other match {
    case that: Cell =>
      (that canEqual this) &&
        pos == that.pos &&
        terrain == that.terrain
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(pos, terrain)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

class Combatant(pos: Position, var ap: Int, var hp: Int, symbol: String) extends Cell(pos, Terrain.UNIT, symbol) {

  def attack(otherCombatant: Combatant): Combatant = {
    otherCombatant.hp = otherCombatant.hp - this.ap
    if (otherCombatant.hp < 0) otherCombatant else null
  }

  def display(): String = {
    this match {
      case _: Elf => s"E(${this.hp})"
      case _: Goblin => s"G(${this.hp})"
      case _ => ""
    }
  }

  def identifyTargets(potentialTargets: List[Combatant]): List[Combatant] = {
    this match {
      case _: Elf =>
        potentialTargets.filter {
          case g: Goblin => true
          case _ => false
        }
      case _: Goblin =>
        potentialTargets.filter {
          case e: Elf => true
          case _ => false
        }
      case _ =>
        List()
    }
  }

  override def toString = s"Combatant(${this.pos} ${this.ap} ${this.hp})"
}

class Elf(pos: Position, ap: Int, hp: Int) extends Combatant(pos, ap, hp, "E") {
  override def toString: String = s"Elf(${this.pos} ${this.ap} ${this.hp})"
}

class Goblin(pos: Position, ap: Int, hp: Int) extends Combatant(pos, ap, hp, "G") {
  override def toString: String = s"Goblin(${this.pos} ${this.ap} ${this.hp})"
}

class Cave(val landscape: Array[Array[Cell]],
           val combatants: List[Combatant]) {

  def combatantsAtLine(line: Int): String = {
    "     " + this.combatants
      .filter(c => c.pos.y == line)
      .map(c => c.display())
      .mkString(", ")
  }

  def resetAfterSearch(): Unit = {
    for (line <- this.landscape) {
      for (cell <- line) {
        cell.level = -1
        cell.parents = null
      }
    }
  }

  def display(): String = {
    this.landscape.zipWithIndex.map(input => {
      val cells = input._1
      val idx = input._2
      cells.map {
        case _: Elf => "E"
        case _: Goblin => "G"
        case c: Cell => if (c.terrain == Terrain.OPEN) "." else "#"
      }.mkString("") + combatantsAtLine(idx)
    }).mkString("\n")
  }

  def displayPath(path: List[Cell]): String = {
    val pathMap = path.map(p => p.pos -> p).toMap
    this.landscape.zipWithIndex.map(input => {
      val cells = input._1
      val i = input._2
      cells.zipWithIndex.map(input => {
        val j = input._2
        val cell = input._1
        val curPos = new Position(j, i)

        if (pathMap.contains(curPos)) {
          "X"
        } else {
          cell match {
            case _: Elf => "E"
            case _: Goblin => "G"
            case c: Cell => if (c.terrain == Terrain.OPEN) "." else "#"
          }
        }
      }).mkString("")
    }).mkString("\n")
  }

  def swapCells(cell1: Cell, cell2: Cell): Unit = {
    val cell1pos = cell1.pos

    cell1.pos = cell2.pos
    cell2.pos = cell1pos

    this.landscape(cell1.pos.y)(cell1.pos.x) = cell1
    this.landscape(cell2.pos.y)(cell2.pos.x) = cell2
  }
}


object Aoc15 {
  def test_file = "../inputs/input-15.0.txt"

  def test_file2 = "../inputs/input-15.2.txt"

  def real_file = "../inputs/input-15.1.txt"

  def readFile(fileName: String): Array[Array[String]] = {
    Source.fromFile(fileName).getLines.toArray.map(line => line.split(""))
  }

  val defaultHp = 200
  val defaultAp = 3

  def caveFromFile(fileName: String): Cave = {
    val raw = Aoc15.readFile(fileName)
    var combatants = new ListBuffer[Combatant]

    val landscape: Array[Array[Cell]] = raw.zipWithIndex.map {
      case (line, i) => line.zipWithIndex.map {
        case (chr, j) => {
          val terrainUnit = convertCharToTerrainUnit(chr, j, i)
          terrainUnit match {
            case combatant: Combatant =>
              combatants = combatants += combatant
            case _ =>
          }

          terrainUnit
        }
      }
    }
    new Cave(landscape, combatants.toList)
  }

  def adjacentOpenCells(cell: Cell, cells: Array[Array[Cell]]): List[Cell] = {
    val height = cells.length
    val width = cells(0).length
    val x = cell.pos.x
    val y = cell.pos.y

    List((x, y + 1), (x, y - 1), (x - 1, y), (x + 1, y))
      .filter(p => {
        val (a, b) = p
        (a >= 0 || a < width) && (b >= 0 || b < height)
      }).map(p => {
      cells(p._2)(p._1)
    }).filter(c => c.terrain == Terrain.OPEN)
  }

  def convertCharToTerrainUnit(value: String, x: Int, y: Int): Cell = {
    val pos = new Position(x, y)

    if (value == "#") {
      new Cell(pos, Terrain.WALL, value)
    } else if (value == ".") {
      new Cell(pos, Terrain.OPEN, value)
    } else if (value == "G") {
      new Goblin(pos, defaultAp, defaultHp)
    } else if (value == "E") {
      new Elf(pos, defaultAp, defaultHp)
    } else {
      null
    }
  }

  // Getting a stack overflow here so something is obviously failing
  def dfs(n: Cell, result: ListBuffer[List[Cell]], path: ListBuffer[Cell]): Unit = {
    path.prepend(n)

    if (n.parents.isEmpty) { // base case: we came to target vertex
      result += path.toList
    }
    for (p <- n.parents) {
      dfs(p, result, path)
    }
    // do not forget to remove the processed element from path
    path.remove(0)
  }


  //  def constructPath(node: Cell): List[Cell] = {
  //    var path = new ListBuffer[Cell]()
  //    var current = node
  //    while ( {
  //      current.pathParent != null
  //    }) {
  //      path += current
  //      current = current.pathParent
  //    }
  //
  //    path.toList
  //  }


  //  // totally ripped off from: http://www.peachpit.com/articles/article.aspx?p=101142
  //  def old_search(startNode: Cell, goal: Position, cave: Cave): List[Cell] = { // list of visited nodes
  //    val closedList = new ListBuffer[Cell]
  //    // list of nodes to visit (sorted)
  //    val openList = new mutable.Queue[Cell]()
  //    startNode.pathParent = null
  //    openList.enqueue(startNode)
  //
  //    while (openList.nonEmpty) {
  //      val node = openList.dequeue()
  //      if (node.pos == goal) { // path found!
  //        return constructPath(node)
  //      }
  //      else {
  //        closedList += node
  //        // add neighbors to the open list
  //        val i = adjacentOpenCells(node, cave.landscape).iterator
  //        while (i.hasNext) {
  //          val neighborNode = i.next
  //          if (!closedList.contains(neighborNode) && !openList.contains(neighborNode)) {
  //            neighborNode.pathParent = node
  //            openList += neighborNode
  //          }
  //        }
  //      }
  //    }
  //    // no path found
  //    null
  //  }

  def getPaths(node: Cell): List[List[Cell]] = {
    getPathsRecur(node, List())
  }

  def getPathsRecur(node: Cell, path: List[Cell]): List[List[Cell]] = {
    val working_path = node :: path

    if (node.parents.isEmpty) {
      List(working_path)
    } else {
      node.parents.toList.flatMap(p => getPathsRecur(p, working_path))
    }
  }

  // https://ideone.com/UluCBb
  def search(startNode: Cell, goal: Position, cave: Cave): List[List[Cell]] = { // list of visited nodes
    val closedList = new ListBuffer[Cell]
    // list of nodes to visit (sorted)
    val openList = new mutable.Queue[Cell]()
    startNode.parents = new ListBuffer[Cell]
    startNode.level = 0

    openList.enqueue(startNode)

    while (openList.nonEmpty) {
      val node = openList.dequeue()
      if (node.pos == goal) { // path found!
        //        val result = new ListBuffer[List[Cell]]()
        //        val path = new ListBuffer[Cell]()
        //        dfs(node, result, path)
        //        return result
        return getPaths(node)
      }
      else {
        closedList += node
        // add neighbors to the open list
        val i = adjacentOpenCells(node, cave.landscape).iterator
        while (i.hasNext) {
          val neighborNode = i.next
          if (!closedList.contains(neighborNode)) {
            if (!openList.contains(neighborNode)) {
              openList += neighborNode
            }
            neighborNode.addParent(node)
          }
        }
      }
    }
    // no path found
    null
  }

  def otherSearch(cave: Cave, start: Cell, targets: List[Cell]): (ListBuffer[Cell], Int) = {
    val target_set: Set[Cell] = targets.toSet
    var visited = Set[Cell]()
    var min_distance: Int = -1
    var queue: Vector[(Cell, Int)] = Vector((start, 0))
    val closest_nodes = new ListBuffer[Cell]

    while (queue.nonEmpty) {
      val (cell, distance) = queue.take(1)(0)
      queue = queue.drop(1)

      if (min_distance != -1 && distance > min_distance) {
        return (closest_nodes, min_distance)
      }

      if (!visited.contains(cell)) {
        visited = visited + cell

        if (target_set.contains(cell)) {
          min_distance = distance
          closest_nodes += cell
        }

        for (n <- adjacentOpenCells(cell, cave.landscape)) {
          if (!visited.contains(n)) {
            queue = queue :+ (n, distance + 1)
          }
        }
      }
    }

    (closest_nodes, min_distance)
  }

  def readingOrder[C <: Cell](a: C): Int = {
    a.pos.y * 9999 + a.pos.x
  }

  def compareReadingOrder[C <: Cell](a: C, b: C): Boolean = {
    readingOrder(a) < readingOrder(b)
  }

  def sortReadingOrder[C <: Cell](cells: List[C]): List[C] = {
    cells.sortWith((a, b) => {
      compareReadingOrder(a, b)
    })
  }

  def turn(combatant: Combatant, cave: Cave): Unit = {
    //    println(cave.display())
    val targets: List[Combatant] = combatant.identifyTargets(cave.combatants)
    if (targets.isEmpty) return

    val victims = targets.filter(c => c.adjacent(combatant)).sortBy(_.hp)
    if (victims.isEmpty) {
      //      val openCells: List[Cell] = sortReadingOrder(targets.flatMap(target => adjacentOpenCells(target, cave.landscape)))
      //      val reachableCells: List[List[Cell]] = openCells.flatMap(cell => search(combatant, cell.pos, cave)).filter(p => p != null)
      //
      //      if (reachableCells.isEmpty) return
      //
      //      val selectedPathLen = reachableCells.minBy(_.length).length
      //      val potentialPaths = reachableCells.filter(p => p.length == selectedPathLen)
      //      val selectedPath = potentialPaths.zip(potentialPaths.map(p => p.head)).sortWith((a, b) => {
      //        compareReadingOrder(a._2, b._2)
      //      }).head._1
      //
      //      println(cave.displayPath(selectedPath))
      //      cave.swapCells(combatant, selectedPath.last)


      val openCells: List[Cell] = sortReadingOrder(targets.flatMap(target => adjacentOpenCells(target, cave.landscape)))
      val (closestReachableCells, minDist) = otherSearch(cave, combatant, openCells)

      if (closestReachableCells.isEmpty) {
        return
      }

      val selectedCell = closestReachableCells.toList.minBy(readingOrder)
      val nearbyCells = sortReadingOrder(adjacentOpenCells(combatant, cave.landscape))

      var nextCell: Cell = null
      for (c <- nearbyCells) {
        val (_, distance) = otherSearch(cave, c, List(selectedCell))
        if (distance == minDist - 1  && nextCell == null) {
          nextCell = c
        }
      }

      cave.swapCells(combatant, nextCell)


    } else {
      val victim = victims.head
      val killed = combatant.attack(victim)

      if (killed != null) {
        // TODO handle kills
      }
    }


  }

  def round(roundNum: Int, cave: Cave): Int = {
    val orderedCombatants = sortReadingOrder(cave.combatants)
    orderedCombatants.foreach(cell => turn(cell, cave))
    roundNum + 1
  }

  def partOne(): Unit = {
    val cave = caveFromFile(test_file)
    println(cave.display())
    for (a <- 1 to 3) {
      round(a, cave)
      println(cave.display())
    }

    //    val root = new Cell(new Position(0, 0), Terrain.OPEN, ".")
    //    val child1 = new Cell(new Position(1, 1), Terrain.OPEN, ".")
    //    val child2 = new Cell(new Position(2, 2), Terrain.OPEN, ".")
    //
    //    root.parents += child1
    //    root.parents += child2
    //
    //    val result = getPaths(root)
    //
    //    println(result)
  }

  def main(args: Array[String]): Unit = {
    this.partOne()
  }
}







