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

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
          other is Point &&
              runtimeType == other.runtimeType &&
              x == other.x &&
              y == other.y;

  @override
  int get hashCode =>
      x.hashCode ^
      y.hashCode;
}

class Cart{
  num x;
  num y;
  String char;
  num dir;

  Cart(num x, num y, String char){
    this.x = x;
    this.y = y;
    this.char = char;
    this.dir = 0;
  }

  static compare(Cart a, Cart b){
    return (10000 * a.y + a.x).compareTo(10000 * b.y + b.x);
  }

}

class Grid{
  Map<Point, String> track;
  List<Cart> carts;


  Grid(this.track, this.carts);

  static Grid fromFile(String filename){
    var file = new File(filename);
    var input = file.readAsStringSync();
    var lines = input.split("\n");
    var colSize = lines.length;

    final cartToTrack = const {'>':'-','<':'-','^':'|','v':'|'};

    Map<Point, String> track = new Map();
    List<Cart> carts = [];

    for(num i=0;i<colSize;i++){
      var lineStr = lines[i];
      var line = lineStr.split("");
      for(num j=0;j<line.length;j++){
        String char = line[j];
        Point p = new Point(j, i);

        if(cartToTrack.keys.contains(char)){
          // Cart
          carts.add(new Cart(j, i, char));
          track[p] = cartToTrack[char];
        } else {
          // Not Cart
          track[p] = char;
        }
      }
    }

    return Grid(track, carts);
  }
}

String testFile = "../inputs/input-13.0.txt";
String realFile = "../inputs/input-13.1.txt";

partOneTwo(){
  var grid = Grid.fromFile(realFile);
//  print(grid);
  var no_crashes = true;
  var keepTicking = true;
  var hadTickAtOne = false;


  while(keepTicking){
    var crashSites = [];
    for(Cart cart in grid.carts){
        Point next;
        if(crashSites.contains(new Point(cart.x, cart.y))){
          print("cont");
          continue;
        }
        if(cart.char == "^"){
          next = new Point(cart.x, cart.y - 1);
        } else if(cart.char == ">"){
          next = new Point(cart.x + 1, cart.y);
        } else if(cart.char == "v"){
          next = new Point(cart.x, cart.y + 1);
        } else if(cart.char == "<"){
          next = new Point(cart.x - 1, cart.y);
        }

        if(grid.carts.any((c) => c.x == next.x && c.y == next.y)){
          // CRASH
          // Uncomment for part 1
//          print(next);
//          no_crashes = false;
          crashSites.add(next);
        }

        String nextChar;

        if(grid.track[next].contains("\\") || grid.track[next].contains("/")){
          // TURN
          var move = cart.char + grid.track[next];
          switch(move){
            case '>\\':
              nextChar =  'v';
              break;
            case '<\\':
              nextChar =  '^';
              break;
            case '^\\':
              nextChar =  '<';
              break;
            case 'v\\':
              nextChar =  '>';
              break;
            case '>/':
              nextChar = '^';
              break;
            case '</':
              nextChar = 'v';
              break;
            case '^/':
              nextChar = '>';
              break;
            case 'v/':
              nextChar = '<';
              break;
          }
        } else if(grid.track[next] == "+"){
            var dirs = ['<','^','>','v'];
            nextChar = dirs[(dirs.indexOf(cart.char) + (cart.dir - 1)) % 4];
            cart.dir = (cart.dir + 1) % 3;
        }

        cart.x = next.x;
        cart.y = next.y;
//        print("${cart.x}, ${cart.y}, ${cart.char} -> ${nextChar}");
        if(nextChar != null){cart.char = nextChar;}
    }

    if(crashSites.length != 0){
      grid.carts = grid.carts.where((cart) => !crashSites.contains(new Point(cart.x, cart.y))).toList();
    }

    if(grid.carts.length == 1) {
      keepTicking = false;
    } else{
      grid.carts.sort((a,b) => Cart.compare(a,b));
    }
  }

  print("${grid.carts[0].x}, ${grid.carts[0].y}");
}

main() {
  partOneTwo();
}
