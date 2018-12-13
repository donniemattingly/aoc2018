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

  String toString(){
    return "Point{x: $x, y: $y}";
  }
}

class Cart {
  // 0 = left, 1 = up, 2 = right, 3 = down
  num direction;
  num nextIntersectionChoice;

  Path currentPath;

  void left() => this.direction = 0;
  void up() => this.direction = 1;
  void right() => this.direction = 2;
  void down() => this.direction = 3;

  num turn(){
     var currentChoice = nextIntersectionChoice;
     this.nextIntersectionChoice = (currentChoice + 1) % 3;

     return currentChoice;
  }
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

  String toString(){
    return "Path{coords:${coords.toString()}, char:$char}";
  }
}

class Grid {
  num width;
  num height;

  List<Cart> carts;
  List<List<Path>> paths;
  List<List<String>> input;
}

Path charToNode(String char, Point loc){
  var path = Path(char, loc);
  return path;
}

String getInput(filename){
  var file = new File(filename);
  return file.readAsStringSync();
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

  for(var i = 0; i < grid.height; i++){
    for(var j=0; j < grid.width; j++){
      var char = charArray[i][j];
      var coords = Point(i, j);
      var path = Path(char, coords);

      pathGrid[i][j] = path;
    }
  }

  List<Cart> carts = [];

  for(var i = 0; i < grid.height; i++){
    for(var j = 0; j < grid.width; j++){
      var currentPath = pathGrid[i][j];
      var t = i == 0 ? null : pathGrid[i-1][j];
      var b = i == grid.height - 1 ? null : pathGrid[i+1][j];
      var l = j == 0 ? null : pathGrid[i][j-1];
      var r = j == grid.width - 1 ? null : pathGrid[i][j+1];

      switch(currentPath.char){
        case ' ':
          currentPath = null;
          break;
        case '|':
          currentPath.t = t;
          currentPath.b = b;
          break;
        case '-':
          currentPath.l = l;
          currentPath.r = r;
          break;
        case "/":
          if(r?.char == '-'){
            currentPath.r = r;
            currentPath.b = b;
          } else {
            currentPath.l = l;
            currentPath.t = t;
          }
          break;
        case "\\":
          if(l?.char == '-'){
            currentPath.l = l;
            currentPath.b = b;
          } else{
            currentPath.r = r;
            currentPath.t = t;
          }
          break;
        case "+":
          currentPath.l = l;
          currentPath.r = r;
          currentPath.b = b;
          currentPath.t = t;
          break;
        case '^':
          currentPath.t = t;
          currentPath.b = b;
          currentPath.char = '|';

          var cart = new Cart();
          cart.up();
          cart.currentPath = currentPath;
          carts.add(cart);
          break;
        case '>':
          currentPath.l = l;
          currentPath.r = r;
          currentPath.char = '-';

          var cart = new Cart();
          cart.right();
          cart.currentPath = currentPath;
          carts.add(cart);
          break;
        case 'v':
          currentPath.t = t;
          currentPath.b = b;
          currentPath.char = '|';

          var cart = new Cart();
          cart.down();
          cart.currentPath = currentPath;
          carts.add(cart);
          break;
        case '<':
          currentPath.l = l;
          currentPath.r = r;
          currentPath.char = '-';

          var cart = new Cart();
          cart.right();
          cart.currentPath = currentPath;
          carts.add(cart);
          break;
      }
    }
  }

  grid.carts = carts;
  grid.paths = pathGrid;

  return grid;
}

void sortCartExecutionOrder(List<Cart> carts){
  carts.sort((a, b){
    num y = a.currentPath.coords.y.compareTo(b.currentPath.coords.y);
    num x = a.currentPath.coords.x.compareTo(b.currentPath.coords.x);

    if(y != 0){
      return y;
    } else{
      return x;
    }
  });
}

void tick(Grid grid){
  sortCartExecutionOrder(grid.carts);

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