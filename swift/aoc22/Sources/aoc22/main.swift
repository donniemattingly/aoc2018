//import SwiftGraph

// stealing a bunch of algorithms from https://github.com/raywenderlich/swift-algorithm-club

struct Terrain {
    var geo: [[Int]]
    var depth: Int
}

func generateTerrain(x: Int, y: Int, depth: Int) -> Terrain {
    var geo = Array(repeating: Array(repeating: 0, count: x + 1), count: y + 1)

    for i in 0...x {
        for j in 0...y {
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

func printTerrain(terrain: Terrain) {
    let repr = terrain.geo.map { rows -> String in
        rows.map { erosionLevel -> String in
            let cell = erosionLevel % 3
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
    }.joined(separator: "\n")

    print(repr)
}

func getRiskLevel(terrain: Terrain, x: Int, y: Int) -> Int {
    var riskLevel = 0
    for i in 0...x {
        for j in 0...y {
            riskLevel += (terrain.geo[j][i] % 3)
        }
    }

    return riskLevel
}

struct Input {
    var x: Int
    var y: Int
    var depth: Int
}

func partOne(input: Input) {
    let terrain = generateTerrain(x: input.x, y: input.y, depth: input.depth)
    let riskLevel = getRiskLevel(terrain: terrain, x: input.x, y: input.y)
    print("Part One: \(riskLevel)")
}

func getPathCost(v: Vertex) -> Int {
    print("(\(v.point.x), \(v.point.y))")
    if let (p, cost) = v.parent {
//        print("\(v.description) <- \(p.description)")

//        print("(\(p.point.x), \(p.point.y))")
        return  getPathCost(v: p) + cost
    } else {
        return 0
    }
}

func partTwo(input: Input) {
    let start = Vertex(p: Point(x: 0, y: 0), type: .Rocky, tool: .Torch)
    let goal = Vertex(p: Point(x: input.x, y: input.y), type: .Rocky, tool: .Torch)

//    guard let result: Vertex = Dijkstra.uniformCostSearch(from: start, to: goal, depth: input.depth) else {
//        print("whoops")
//        return
//    }

//    printGenTerrain(height: input.y + 5, width: input.x + 5, target: goal.point, depth: input.depth)

    print("\n-----\n")
//    let foo = getPathCost(v: result)
//    print(result.parent?.1)

    let result = Dijkstra.findShortestPaths(from: start, to: goal, depth: input.depth)

    print(result?.pathLengthFromStart)
}

let testInput = Input(x: 10, y: 10, depth: 510)
let realInput = Input(x: 7, y: 701, depth: 11394)

//partOne(input: realInput)
partTwo(input: realInput)