@Grab(group='org.codehaus.gpars', module='gpars', version='1.2.1')
import groovy.transform.Memoized

import java.util.concurrent.atomic.AtomicInteger

testSerialNumber = 0
serialNumber = 18

@Memoized
def calculateFuelCellPowerLevel(int x, int y, int serialNumber) {
    def rackId = x + 10
    def raw = ((rackId * y) + serialNumber) * rackId
    def powerlevel = (Math.floorDiv(raw, 100) % 10) - 5
    return powerlevel
}

@Memoized
def caculateFuelCellGridPowerLevel(int x, int y, int serialNumber, int squareSize) {
    def powerLevel = 0
    for (int i = 0; i < squareSize; i++) {
        for (int j = 0; j < squareSize; j++) {
            powerLevel += calculateFuelCellPowerLevel(x + i, y + j, serialNumber)
        }
    }

    return powerLevel
}

@Memoized
def getLargestPowerTotal(int serialNumber, int squareSize) {
    def max = 0
    def maxX = 0
    def maxY = 0
    for (int i = 1; i < 301; i++) {
        for (int j = 1; j < 301; j++) {
            total = caculateFuelCellGridPowerLevel(i, j, serialNumber, squareSize)

            if (total > max) {
                max = total
                maxX = i
                maxY = j
            }
        }
    }

    return [max, maxX, maxY]
}

def partOne() {
    println(getLargestPowerTotal(7400, 3))
}

def partTwo() {
    AtomicInteger max = new AtomicInteger(0)
    def maxSize = new AtomicInteger(0)
    def maxX = new AtomicInteger(0)
    def maxY = new AtomicInteger(0)

//    Integer[] ran = (1..300).toArray()
//
//    GParsPool.withPool {
//
//        ran.eachParallel {
//            def res = getLargestPowerTotal(7400, it)
//            println("on: ${it}")
//            myMax = max.get()
//            if (res[0] > myMax) {
//                max.set(res[0])
//                maxSize.set(it)
//                maxX.set(res[1])
//                maxY.set(res[2])
//            }
//        }
//    }

//    GParsPool.withPool {
//        final List result = [1, 2, 3, 4, 5].collectParallel{it * 2}
//
//        assert ([2, 4, 6, 8, 10].equals(result))
//    }

    for(int i = 1; i < 301; i++){
        println("on: ${i}")
        res = getLargestPowerTotal(7400, i)

        if(res[0] > max.get()){
            max.set(res[0])
            maxSize.set(i)
            maxX.set(res[1])
            maxY.set(res[2])
        }
    }

    println("max: ${max.get()} maxSize: ${maxSize.get()} loc: ${maxX.get()}, ${maxY.get()}")
}

partTwo()