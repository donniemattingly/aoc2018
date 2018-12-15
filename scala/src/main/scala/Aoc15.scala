import Terrain.Terrain

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Terrain extends Enumeration {
  type Terrain = Value
  val WALL, OPEN, UNIT = Value
}

class Cell(var pos: (Int, Int), var terrain: Terrain, val symbol: String) {
  var pathParent: Cell = _

  def adjacent(otherCell: Cell): Boolean = {
    (Math.abs(this.pos._1 - otherCell.pos._1) == 1) || (Math.abs(this.pos._2 - otherCell.pos._2) == 1)
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

class Combatant(pos: (Int, Int), var ap: Int, var hp: Int, symbol: String) extends Cell(pos, Terrain.UNIT, symbol) {

  def attack(otherCombatant: Combatant): Combatant = {
    otherCombatant.hp = otherCombatant.hp - this.ap
    if (otherCombatant.hp < 0) otherCombatant else null
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

class Elf(pos: (Int, Int), ap: Int, hp: Int) extends Combatant(pos, ap, hp, "E") {
  override def toString: String = s"Elf(${this.pos} ${this.ap} ${this.hp})"
}

class Goblin(pos: (Int, Int), ap: Int, hp: Int) extends Combatant(pos, ap, hp, "G") {
  override def toString: String = s"Goblin(${this.pos} ${this.ap} ${this.hp})"
}

class Cave(val landscape: Array[Array[Cell]],
           val combatants: List[Combatant]) {

  def display(): String = {
    this.landscape.map(cells => {
      cells.map {
        case _: Elf => "E"
        case _: Goblin => "G"
        case c: Cell => if (c.terrain == Terrain.OPEN) "." else "#"
      }.mkString("")
    }).mkString("\n")
  }

  def swapCells(cell1: Cell, cell2: Cell): Unit = {
    val cell1pos = cell1.pos

    cell1.pos = cell2.pos
    cell2.pos = cell1pos

    this.landscape(cell1.pos._2)(cell1.pos._1) = cell1
    this.landscape(cell2.pos._2)(cell2.pos._1) = cell2
  }
}


object Aoc15 {
  def test_file = "../inputs/input-15.0.txt"

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
          val terrainUnit = convertCharToTerrainUnit(chr, i, j)
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
    val (x, y) = cell.pos

    List((x, y + 1), (x, y - 1), (x - 1, y), (x + 1, y))
      .filter(p => {
        val (a, b) = p
        (a >= 0 || a < width) && (b >= 0 || b < height)
      }).map(p => {
      cells(p._2)(p._1)
    }).filter(c => c.terrain == Terrain.OPEN)
  }

  def convertCharToTerrainUnit(value: String, x: Int, y: Int): Cell = {
    val pos = (x, y)

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

  def constructPath(node: Cell): List[Cell] = {
    var path = new ListBuffer[Cell]()
    var current = node
    while ( {
      current.pathParent != null
    }) {
      path += current
      current = current.pathParent
    }

    path.toList
  }


  // totally ripped off from: http://www.peachpit.com/articles/article.aspx?p=101142
  def search(startNode: Cell, goal: (Int, Int), allCells: Array[Array[Cell]]): List[Cell] = { // list of visited nodes
    val closedList = new ListBuffer[Cell]
    // list of nodes to visit (sorted)
    val openList = new mutable.Queue[Cell]()
    startNode.pathParent = null
    openList.enqueue(startNode)

    while (openList.nonEmpty) {
      val node = openList.dequeue()
      if (node.pos eq goal) { // path found!
        return constructPath(node)
      }
      else {
        closedList += node
        // add neighbors to the open list
        val i = adjacentOpenCells(node, allCells).iterator
        while ( {
          i.hasNext
        }) {
          val neighborNode = i.next
          if (!closedList.contains(neighborNode) && !openList.contains(neighborNode)) {
            neighborNode.pathParent = node
            openList += neighborNode
          }
        }
      }
    }
    // no path found
    null
  }

  def sortReadingOrder[C <: Cell](cells: List[C]): List[C] = {
    cells.sortWith((a, b) => {
      a.pos._2 * 9999 + a.pos._1 < b.pos._2 * 9999 + b.pos._1
    })
  }

  def turn(combatant: Combatant, cave: Cave): Unit = {
//    println(cave.display())
    val targets: List[Combatant] = combatant.identifyTargets(cave.combatants)
    if (targets.isEmpty) return

    val victims = targets.filter(c => c.adjacent(combatant)).sortBy(_.hp)
    if (victims.isEmpty) {
      val openCells: List[Cell] = sortReadingOrder(targets.flatMap(target => adjacentOpenCells(target, cave.landscape)))
      val reachableCells: List[List[Cell]] = openCells.map(cell => search(combatant, cell.pos, cave.landscape)).filter(p => p != null)

      if (reachableCells.isEmpty) return

      val selectedPath = reachableCells.minBy(_.length)

      cave.swapCells(combatant, selectedPath.head)

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
    for (a <- 1 to 10) {
      round(a, cave)
      println(cave.display())
    }
  }

  def main(args: Array[String]): Unit = {
    this.partOne()
  }
}







