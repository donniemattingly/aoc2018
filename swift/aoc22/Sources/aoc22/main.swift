

struct Terrain {
    var geo: [[Int]]
    var depth: Int
}



func generateTerrain(x: Int, y: Int, depth: Int) -> Terrain{
    var geo = Array(repeating: Array(repeating: 0, count: x + 1), count: y + 1)

    for i in 0...x {
        for j in 0...y{
            var val = 0
            if i == x && j == y {
                val = 0
            } else if j == 0 {
                val = i * 16807

            } else if i == 0 {
                val = j * 48271

            } else {
                val = geo[j-1][i] * geo[j][i-1]
            }

            geo[j][i] = ((val + depth) % 20183)
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

partOne(input: realInput)