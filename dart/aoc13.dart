// This is where the app starts executing.
import 'dart:io';
import 'dart:convert';

class Point {
  Point(num x, num y) {
    this.x = x;
    this.y = y;
  }

  num x;
  num y;

  String toString() {
    return "Point{x: $x, y: $y}";
  }
}

class Cart {
  // cardinal direction
  // 0 = left, 1 = up, 2 = right, 3 = down
  num direction;

  // relative direction to turn
  num nextIntersectionChoice = 0;

  Path currentPath;
  List<Path> previousPaths = new List();

  void left() => this.direction = 0;

  void up() => this.direction = 1;

  void right() => this.direction = 2;

  void down() => this.direction = 3;

  num turn() {
    num newDirection = this.direction;
    switch(this.nextIntersectionChoice){
      // turn left
      case 0:
        newDirection = (this.direction - 1) % 4;
        break;
        // go straight
      case 1:
        newDirection = this.direction;
        break;

        // turn right
      case 2:
        newDirection = (this.direction + 1) % 4;
        break;
    }

    this.nextIntersectionChoice = (this.nextIntersectionChoice + 1) % 3;

    this.direction = newDirection;
    return newDirection;
  }

  String toString(){
    return "Cart{char: ${this.char()} direction: ${this.direction} nextTurn: ${this.nextIntersectionChoice} current: ${this.currentPath} last: ${this.previousPaths}}";
  }

  String char() {
    switch(this.direction){
      case 0:
        return "<";
      case 1:
        return "^";
      case 2:
        return ">";
      case 3:
        return "v";
    }

    return "";
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

  Path(String char, Point coords) {
    this.char = char;
    this.coords = coords;
  }

  Path t;
  Path b;
  Path l;
  Path r;

  List<Path> adjacentPaths() => [this.t, this.r, this.b, this.l];

  Path nextPath(num direction) {
    switch (direction) {
      case 0:
        return this.l;
      case 1:
        return this.t;
      case 2:
        return this.r;
      case 3:
        return this.b;
      default:
        return null;
    }
  }

  Cart c;

  bool isIntersection = false;
  bool isTurn = false;

  String toString() {
    return "Path{coords:${coords.toString()}, char:$char}";
  }
}

class Grid {
  num width;
  num height;

  List<Cart> carts;
  List<List<Path>> paths;
  List<List<String>> input;

  String toString(){
    String output = "";

    for(List<Path> pathList in this.paths){
      String line = "";
      for(Path path in pathList){
        Cart onPath = this.carts.firstWhere((cart) => cart.currentPath == path, orElse: () => null);

        if(onPath != null){
          line = line + onPath.char();
        } else{
          line = line + path.char;
        }
      }
      output = output + line + "\n";
    }

    return output;
  }
}

Path charToNode(String char, Point loc) {
  var path = Path(char, loc);
  return path;
}

String getInput(filename) {
  var file = new File(filename);
  return file.readAsStringSync();
}

Grid getGridFromFile(filename) {
  var grid = new Grid();
  var input = getInput(filename);
  var lines = input.split("\n");
  var lineSize = lines[0].length;
  var colSize = lines.length;

  var charArray =
      lines.map((line) => line.padRight(lineSize).split("")).toList();

  grid.input = charArray;
  grid.width = lineSize;
  grid.height = colSize;

  List<List<Path>> pathGrid = new List.generate(
      grid.height, (i) => List.generate(grid.width, (i) => null));

  for (var i = 0; i < grid.height; i++) {
    for (var j = 0; j < grid.width; j++) {
      var char = charArray[i][j];
      var coords = Point(i, j);
      var path = Path(char, coords);

      pathGrid[i][j] = path;
    }
  }

  List<Cart> carts = [];

  for (var i = 0; i < grid.height; i++) {
    for (var j = 0; j < grid.width; j++) {
      var currentPath = pathGrid[i][j];
      var t = i == 0 ? null : pathGrid[i - 1][j];
      var b = i == grid.height - 1 ? null : pathGrid[i + 1][j];
      var l = j == 0 ? null : pathGrid[i][j - 1];
      var r = j == grid.width - 1 ? null : pathGrid[i][j + 1];

      switch (currentPath.char) {
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
          if (r?.char == '-' || r?.char == '+') {
            currentPath.r = r;
            currentPath.b = b;
          } else {
            currentPath.l = l;
            currentPath.t = t;
          }
          currentPath.isTurn = true;
          break;
        case "\\":
          if (l?.char == '-' || l?.char == '+') {
            currentPath.l = l;
            currentPath.b = b;
          } else {
            currentPath.r = r;
            currentPath.t = t;
          }
          currentPath.isTurn = true;
          break;
        case "+":
          currentPath.l = l;
          currentPath.r = r;
          currentPath.b = b;
          currentPath.t = t;
          currentPath.isIntersection = true;
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

void sortCartExecutionOrder(List<Cart> carts) {
  carts.sort((a, b) {
    num aVal = a.currentPath.coords.y + 1000*a.currentPath.coords.x;
    num bVal = b.currentPath.coords.y + 1000*b.currentPath.coords.x;

    return aVal.compareTo(bVal);
  });

  for(Cart cart in carts){
    print(cart);
  }
}

void advanceCart(Cart cart) {
  var currentPath = cart.currentPath;
  Path nextPath;

  if (currentPath.isTurn) {
    switch(cart.direction){
      case 0:
      case 2:
        if(currentPath.b != null){
          nextPath = currentPath.b;
          cart.direction = 3;
        } else {
          nextPath = currentPath.t;
          cart.direction = 1;
        }
        break;
      case 1:
      case 3:
        if(currentPath.l != null){
          nextPath = currentPath.l;
          cart.direction = 0;
        } else {
          nextPath = currentPath.r;
          cart.direction = 2;
        }
        break;
    }
  } else {
    num direction;

    if (currentPath.isIntersection) {
      direction = cart.turn();
    } else {
      direction = cart.direction;
    }

    nextPath = currentPath.nextPath(direction);
  }

  cart.currentPath = nextPath;
  cart.previousPaths.add(currentPath);
}

Point checkForCollision(List<Cart> carts) {
  var testMap = new Map();

  for(Cart cart in carts){
    var current = testMap[cart.currentPath.coords];
    if(current != null){
      return cart.currentPath.coords;
    } else {
      testMap[cart.currentPath.coords] = cart;
    }
  }

  return null;
}

Point tick(Grid grid) {
  sortCartExecutionOrder(grid.carts);
  for(Cart cart in grid.carts){
    advanceCart(cart);
    Point collision = checkForCollision(grid.carts);

    if(collision != null){
      return collision;
    }
  }

  print("\n\n\n\n\n");
  print(grid);

  return null;
}

String testFile = "../inputs/input-13.0.txt";
String realFile = "../inputs/input-13.1.txt";

partOne() {
  var grid = getGridFromFile(testFile);
  Point collisionPoint;
  num tickCount = 0;
  while(collisionPoint == null){
    print(tickCount);
    collisionPoint = tick(grid);
    tickCount++;
  }

  print(grid);
  print(tickCount);
  print(collisionPoint);
}

main() {
  partOne();
}
