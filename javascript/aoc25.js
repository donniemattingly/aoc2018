const fs = require('fs').promises;
const _ = require('lodash');

let readInput = async (filename) => {
    let res = await fs.readFile(filename);
    return res.toString().split('\n').map(x => x.split(',').map(n => Number.parseInt(n)));
};

let partOne = async (filename) => {
    let input = await readInput(filename);
    let distances = [];
    for(let i = 0; i<input.length; i++){
        distances[i] = Array(input.length);
        for(let j = 0; j<input.length; j++){
            distances[i][j] = dist(input[i], input[j])
        }
    }

    let count = 0;

    for (let i = 0;i < distances.length; i++){
        console.log(`--------------- `);
        count += distinctConstellationsInRow(distances, i);
    }

    console.log(count);
};

let distinctConstellationsInRow = (distances, row) =>{
    console.log(distances);
    let count = 0;
    for(let i = 0; i < distances.length; i++){
        if(distances[row][i] <= 3){
            distances[row][i] = NaN;
            count = 1;

            // find and mark other parts of this constellation
            distinctConstellationsInRow(distances, i);
        }
    }

    return count;
};

let dist = (p1, p2) => {
    return _.sumBy(_.zip(p1, p2).map(x => x[0] - x[1]), x => Math.abs(x))
};

let main = async () => {
    let testFile = '../inputs/input-25.0.txt';
    let realFile = '../inputs/input-25.1.txt';

    await partOne(testFile);
};


main();
