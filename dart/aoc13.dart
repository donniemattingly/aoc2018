// This is where the app starts executing.
import 'dart:io';
import 'dart:convert';

class Point{

  Point(num x, num y){
    this.x = x;
    this.y = y;
  }

  num x;
  num y;
}

class Cart {
  // 0 = left, 1 = up, 2 = right, 3 = left
  num direction;
  num nextIntersectionChoice;
}

class PathTemplate {
  Point coords;

  // horizontal, vertical, up-right, up-left, down-left, down-right
  String type;
}

class Path {
  String char;
  Point coords;

  Path(String char, Point coords){
    this.char = char;
    this.coords = coords;
  }

  Path t;
  Path b;
  Path l;
  Path r;
  
  Cart c;
}

class Grid {
  num width;
  num height;

  List<Cart> carts;
  List<List<String>> input;
}

Path charToNode(String char, Point loc){
  var path = Path(char, loc);
  return path;
}

Grid getGridFromFile(filename){
  var grid = new Grid();
  var input = getInput(filename);
  var lines = input.split("\n");
  var lineSize = lines[0].length;
  var colSize = lines.length;

  var charArray = lines
      .map((line) => line.padRight(lineSize).split(""))
      .toList();

  grid.input = charArray;
  grid.width = lineSize;
  grid.height = colSize;


  List<List<Path>> pathGrid =
    new List.generate(grid.height, (i) => List.generate(grid.width, (i) => null));

  print("weight: ${grid.width} height: ${grid.height}");
  print(pathGrid);

  for(var i = 0; i < grid.height; i++){
    for(var j=0; j < grid.width; j++){
      print("i: $i j: $j");
      var char = charArray[i][j];
      var coords = Point(i, j);
      var path = Path(char, coords);

      pathGrid[i][j] = path;
    }
  }

  print(pathGrid);

  return grid;
}

String getInput(filename){
  var file = new File(filename);
  return file.readAsStringSync();
}



String testFile = "../inputs/input-13.0.txt";
String realFile = "../inputs/input-13.1.txt";

partOne(){
  var grid = getGridFromFile(testFile);
  for(var line in grid.input){
    print(line);
  }
}

main() {
  partOne();
}