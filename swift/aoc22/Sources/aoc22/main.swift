import SwiftGraph

struct Terrain {
    var geo: [[Int]]
    var depth: Int
}

func generateTerrain(x: Int, y: Int, depth: Int) -> Terrain{
    var geo = Array(repeating: Array(repeating: 0, count: x + 1), count: y + 1)

    for i in 0...x {
        for j in 0...y{
            var geoIndex = 0
            if i == x && j == y {
                geoIndex = 0
            } else if j == 0 {
                geoIndex = i * 16807

            } else if i == 0 {
                geoIndex = j * 48271

            } else {
                geoIndex = geo[j-1][i] * geo[j][i-1]
            }

            geo[j][i] = ((geoIndex + depth) % 20183)
        }
    }

    return Terrain(geo: geo, depth: depth)
}

func printTerrain(terrain: Terrain){
    let repr = terrain.geo.map { rows -> String in
        rows.map { erosionLevel -> String in
            let cell = erosionLevel % 3
            var sym = ""
            if(cell == 0){
                sym = "."
            } else if(cell == 1){
                sym = "="
            } else {
                sym = "|"
            }

            return sym
        }.joined(separator: "")
    }.joined(separator: "\n")

    print(repr)
}

func getRiskLevel(terrain: Terrain, x: Int, y: Int) -> Int{
    var riskLevel = 0
    for i in 0...x {
        for j in 0...y{
            riskLevel += (terrain.geo[j][i] % 3)
        }
    }

    return riskLevel
}

struct Input{
    var x: Int
    var y: Int
    var depth: Int
}

let testInput = Input(x: 10, y: 10, depth: 510)
let realInput = Input(x: 7, y: 701, depth: 11394)

func partOne(input: Input){
    let terrain = generateTerrain(x: input.x, y: input.y, depth: input.depth)
    let riskLevel = getRiskLevel(terrain: terrain, x: input.x, y: input.y)
    print("Part One: \(riskLevel)")
}

enum TerrainType {
    case Rocky, Wet, Narrow
}

enum Tool {
    case Neither, Torch, Climbing
}

struct Vertex: Equatable {
    var x: Int
    var y: Int
    var type: TerrainType
    var tool: Tool

    static func == (lhs: Vertex, rhs: Vertex) -> Bool {
        return lhs.x == rhs.x &&
                lhs.y == rhs.y &&
                lhs.type == rhs.type &&
                lhs.tool == rhs.tool
    }
}

struct Point: Equatable, Hashable {
    var x: Int
    var y: Int

    static func == (lhs: Point, rhs: Point) -> Bool {
        return lhs.x == rhs.x &&
                lhs.y == rhs.y
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine(x)
        hasher.combine(y)
    }
}

func terrainToVertices(terrain: Terrain) -> ([Vertex], Dictionary<Point, [Vertex]>) {
    // each square is actually two vertices (one for each possible tool)
    let arr = terrain.geo
    var vertices: [Vertex] = []
    var gridDict: Dictionary<Point, [Vertex]> = Dictionary()

    for j in 0..<arr.count {
        for i in 0..<arr[0].count {
            let terrainType = arr[j][i] % 3
            var v: [Vertex] = []
            if(terrainType == 0){
                // Rocky (Climbing / Torch)
                v = [Vertex(x: i, y: j, type: .Rocky, tool: .Climbing), Vertex(x: i, y: j, type: .Rocky, tool: .Torch)]
            } else if(terrainType == 1){
                // Wet (Climbing / Neither)
                v = [Vertex(x: i, y: j, type: .Wet, tool: .Climbing), Vertex(x: i, y: j, type: .Wet, tool: .Neither)]
            } else {
                // Narrow (Torch / Neither)
                v = [Vertex(x: i, y: j, type: .Narrow, tool: .Torch), Vertex(x: i, y: j, type: .Narrow, tool: .Neither)]
            }

            vertices.append(contentsOf: v)
            gridDict[Point(x: i, y: j)] = v
        }
    }

    return (vertices, gridDict)
}

func generateGraph(terrain: Terrain) -> WeightedGraph<Vertex, Int> {
    let (vertices, gridDict) = terrainToVertices(terrain: terrain)

    // add all vertices
    let graph = WeightedGraph<Vertex, Int>(vertices: vertices)

    for j: Int in 0..<terrain.geo.count {
        for i: Int in 0..<terrain.geo[0].count{
            let v = gridDict[Point(x: i, y: j)]!
            let neighbors: [Vertex] = [
                gridDict[i - 1, j],
                gridDict[i + 1, j],
                gridDict[i, j - 1],
                gridDict[i, j + 1]
            ].flatMap { (vs: [Vertex]?) -> [Vertex]? in
                vs ?? []
            }
        }
    }
    return graph
}

func partTwo(input: Input){
    let terrain = generateTerrain(x: input.x, y: input.y, depth: input.depth)
    let graph = generateGraph(terrain: terrain)

    print(graph)
}

//partOne(input: realInput)
partTwo(input: testInput)