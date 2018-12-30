import Foundation

public enum TerrainType {
    case Rocky, Wet, Narrow
}

public enum Tool {
    case Neither, Torch, Climbing
}

public func pointToVertices(point: Point, target: Point, depth: Int) -> [Vertex] {
    let terrainType = geoAtPoint(p: point, target: target, depth: depth) % 3
    if (terrainType == 0) {
        // Rocky (Climbing / Torch)
        return [Vertex(p: point, type: .Rocky, tool: .Climbing), Vertex(p: point, type: .Rocky, tool: .Torch)]
    } else if (terrainType == 1) {
        // Wet (Climbing / Neither)
        return [Vertex(p: point, type: .Wet, tool: .Climbing), Vertex(p: point, type: .Wet, tool: .Neither)]
    } else {
        // Narrow (Torch / Neither)
        return [Vertex(p: point, type: .Narrow, tool: .Torch), Vertex(p: point, type: .Narrow, tool: .Neither)]
    }
}

var cache = [Point: Int]()

public func geoAtPoint(p: Point, target: Point, depth: Int) -> Int {
    let i = p.x
    let j = p.y

    if let result = cache[p] {
        return result
    }

    var geoIndex = 0
    if (i == target.x && j == target.y) || (i == 0 && j == 0) {
        geoIndex = 0
    } else if j == 0 {
        geoIndex = (i * 16807)

    } else if i == 0 {
        geoIndex = (j * 48271)

    } else {
        geoIndex = (geoAtPoint(p: Point(x: i - 1, y: j), target: target, depth: depth) * geoAtPoint(p: Point(x: i, y: j - 1), target: target, depth: depth))
    }

    let result = (geoIndex + depth) % 20183
    cache[p] = result

    return result
}

func printGenTerrain(height: Int, width: Int, target: Point, depth: Int) {
    print((0...height).map { j in
        (0...width).map { i in
            let cell = geoAtPoint(p: Point(x: i, y: j), target: target, depth: depth) % 3
            var sym = ""
            if (cell == 0) {
                sym = "."
            } else if (cell == 1) {
                sym = "="
            } else {
                sym = "|"
            }

            return sym
        }.joined(separator: "")
    }.joined(separator: "\n"))
}

open class Vertex: Equatable, Hashable, CustomStringConvertible {
    var point: Point
    var type: TerrainType
    var tool: Tool

    open var pathLengthFromStart = Double.infinity
    open var pathVerticesFromStart: [Vertex] = []

    var parent: (Vertex, Int)?

    public static func ==(lhs: Vertex, rhs: Vertex) -> Bool {
        return lhs.point == rhs.point &&
                lhs.type == rhs.type &&
                lhs.tool == rhs.tool
    }

    public func hash(into hasher: inout Hasher) {
        hasher.combine(point)
        hasher.combine(type)
        hasher.combine(tool)
    }

    public private(set) var description: String

    public init(p: Point, type: TerrainType, tool: Tool) {
        self.point = p
        self.type = type
        self.tool = tool

        self.description = "(\(point.x), \(point.y)) \(tool) \(type)"
    }


    // each square has two nodes (for each tool type)
    public func neighborPoints() -> [Point] {
        let x = self.point.x
        let y = self.point.y

        return [
            Point(x: x, y: y),
            Point(x: x - 1, y: y),
            Point(x: x + 1, y: y),
            Point(x: x, y: y - 1),
            Point(x: x, y: y + 1)]
                .filter({ $0.isValid() })
    }

    public func neighbors(goal: Point, depth: Int) -> [(Vertex, Int)] {
        let neighborPoints = self.neighborPoints()

        let neighbors = neighborPoints
                .flatMap {
                    pointToVertices(point: $0, target: goal, depth: depth)
                }
                .filter {
                    $0 != self
                } // want the tool change neighbor
                .map {
                    ($0, $0.point == self.point ? 7 : 1)
                }

        return neighbors
    }
}

public struct Point: Equatable, Hashable {
    var x: Int
    var y: Int

    public static func ==(lhs: Point, rhs: Point) -> Bool {
        return lhs.x == rhs.x &&
                lhs.y == rhs.y
    }

    public func hash(into hasher: inout Hasher) {
        hasher.combine(x)
        hasher.combine(y)
    }

    // only positive positions are valid
    public func isValid() -> Bool {
        return x >= 0 && y >= 0
    }
}

public class Dijkstra {
    // from https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Practical_optimizations_and_infinite_graphs
    public static func uniformCostSearch(from startVertex: Vertex, to goalVertex: Vertex, depth: Int) -> Vertex? {
        // min priority Queue of edges
        var frontier = PriorityQueue<(Vertex, Int)>(sort: { $0.1 < $1.1 });
        frontier.enqueue((startVertex, 0))

        var explored = Set<Vertex>()

        while true {
            guard let (currentVertex, weight) = frontier.dequeue() else {
                break
            }

//            print(weight)
//            print("x: \(currentVertex.point.x) y: \(currentVertex.point.y) tool: \(currentVertex.tool) type: \(currentVertex.type)")

            if currentVertex.point == goalVertex.point && currentVertex.tool == .Torch {
                return currentVertex
            }

            let neighbors = currentVertex.neighbors(goal: goalVertex.point, depth: depth)
            let validNeighbors = neighbors
                    .filter({ !explored.contains($0.0) })
                    .filter { (neighbor: Vertex, w: Int) -> Bool in
                        neighbor.tool == currentVertex.tool || neighbor.point == currentVertex.point
                    }

//            print("\n---------Current---------")
//            print((currentVertex, weight))
//
//            print("\n---------Neighbors---------")
//            for n in neighbors {
//                print("\(n) valid: \(validNeighbors.contains(where: { $0 == n }))")
//            }
//            let changes = validNeighbors.filter { (vertex: Vertex, i: Int) -> Bool in
//                vertex.point == currentVertex.point
//            }

            for (n, w) in validNeighbors {
                n.parent = (currentVertex, w + weight)
                frontier.enqueue((n, w + weight))
                explored.insert(n)
            }
        }

        return nil
    }

    static public func findShortestPaths(from startVertex: Vertex, to goalVertex: Vertex, depth: Int) -> Vertex? {


        var currentVertices =
                Set((0...(goalVertex.point.y + 10)).flatMap { j in
                    (0...(goalVertex.point.x + 10)).flatMap { i in
                        pointToVertices(point: Point(x: i, y: j), target: goalVertex.point, depth: depth)
                    }
                })

        startVertex.pathLengthFromStart = 0
        startVertex.pathVerticesFromStart.append(startVertex)

        var paths: [Vertex] = []
        var currentVertex: Vertex? = startVertex
        while let vertex = currentVertex {
            currentVertices.remove(vertex)
//            print(currentVertices.count)

            if vertex.point == goalVertex.point && vertex.tool == .Torch {
                paths.append(vertex)
            }

            let filteredNeighbors = vertex.neighbors(goal: goalVertex.point, depth: depth).filter { (v: Vertex, w) in
                currentVertices.contains(v) && (v.tool == currentVertex?.tool || v.point == currentVertex?.point)
            }

            for neighbor in filteredNeighbors {
                let neighborVertex = neighbor.0
                let weight = neighbor.1

                let theoreticNewWeight = vertex.pathLengthFromStart + Double(weight)
                if theoreticNewWeight < neighborVertex.pathLengthFromStart {
//                    print(theoreticNewWeight)
                    neighborVertex.pathLengthFromStart = theoreticNewWeight
                    neighborVertex.pathVerticesFromStart = vertex.pathVerticesFromStart
                    neighborVertex.pathVerticesFromStart.append(neighborVertex)

                    let cur : Vertex = currentVertices.remove(neighborVertex)!

                    if(cur.pathLengthFromStart < neighborVertex.pathLengthFromStart){
                        currentVertices.insert(cur)
                    } else{
                        currentVertices.insert(neighborVertex)
                    }
                }

            }

            if currentVertices.isEmpty {
                currentVertex = nil
                break
            }

//            for v in currentVertices {
//                if v.pathLengthFromStart != Double.infinity {
//                    print(v.pathLengthFromStart)
//                }
//            }

            currentVertex = currentVertices.min {
                $0.pathLengthFromStart < $1.pathLengthFromStart
            }
        }

//        return currentVertices.first { vertex in
//            vertex == goalVertex
//        }

        return paths.min {
            $0.pathLengthFromStart < $1.pathLengthFromStart
        }
    }

    static public func findShortestPathsPQ(from startVertex: Vertex, to goalVertex: Vertex, depth: Int) -> Vertex? {


//        var currentVertices =
//                Set((0...(goalVertex.point.y+10)).flatMap { j in
//                    (0...(goalVertex.point.x+10)).flatMap { i in
//                        pointToVertices(point: Point(x: i, y: j), target: goalVertex.point, depth: depth)
//                    }
//                })


        var currentVerticesQ = HashedHeap<Vertex>(sort: {
            $0.pathLengthFromStart < $1.pathLengthFromStart
        })

        currentVerticesQ.insert(startVertex)

//        for vertex in currentVertices{
//            currentVerticesQ.enqueue(vertex)
//        }

        startVertex.pathLengthFromStart = 0
        startVertex.pathVerticesFromStart.append(startVertex)
        var currentVertex: Vertex? = startVertex
        var visited = Set<Vertex>()

        while let vertex = currentVerticesQ.remove() {
            visited.insert(vertex)

            if vertex.point == goalVertex.point && vertex.tool == .Torch {
                return currentVertex
            }

            let filteredNeighbors = vertex.neighbors(goal: goalVertex.point, depth: depth).filter { (v: Vertex, w) in
                !visited.contains(v) && (v.tool == currentVertex?.tool || v.point == currentVertex?.point)
            }

            for neighbor in filteredNeighbors {
                let neighborVertex = neighbor.0
                let weight = neighbor.1

                let theoreticNewWeight = vertex.pathLengthFromStart + Double(weight)

//                    print(theoreticNewWeight)
                neighborVertex.pathLengthFromStart = theoreticNewWeight
                neighborVertex.pathVerticesFromStart = vertex.pathVerticesFromStart
                neighborVertex.pathVerticesFromStart.append(neighborVertex)

                if let idx = currentVerticesQ.index(of: neighborVertex){
                    currentVerticesQ.replace(neighborVertex, at: idx)
                }

            }

//            if currentVertices.isEmpty {
//                currentVertex = nil
//                break
//            }

//            for v in currentVertices {
//                if v.pathLengthFromStart != Double.infinity {
//                    print(v.pathLengthFromStart)
//                }
//            }

//            currentVertex = currentVertices.min {
//                $0.pathLengthFromStart < $1.pathLengthFromStart
//            }
        }

//        return currentVertices.first { vertex in vertex == goalVertex }

        return nil
    }
}