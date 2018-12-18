import strutils
import sequtils
import strformat
import tables

var board: seq[seq[char]] = @[]
let tiles = ['.', '|', '#']

for line in lines "../inputs/input-18.1.txt":
  var board_line: seq[char] = @[]
  for val in line:
    board_line.add(val)
  board.add(board_line)


proc printBoard(board: array[50, array[50, char]]) =
  var output = ""
  for line in board:
    var rowStr = ""
    for chr in line:
      rowStr = rowStr & chr
    output = output & rowStr & "\n"
  echo output

proc neighbors(x: int, y: int, b: array[50, array[50, char]]): seq[char] =
  var points = [
    [x-1, y-1],
    [x, y-1],
    [x+1, y-1],
    [x-1, y],
    [x+1, y],
    [x-1, y+1],
    [x, y+1],
    [x+1, y+1],
    ]
  var neighbors: seq[char] = @[]

  for point in points:
    try:
      neighbors.add(b[point[0]][point[1]])
    except IndexError:
      discard "foo"
      
  return neighbors


var neighbors = array[8, char]
proc fastRestul(x: int, y: int, old_board: array[50, array[50, char]]) =
  
      
proc acreResult(acre: char, neighbors: seq[char]): char =
  if(acre == '.'):
    if(count(neighbors, '|') >= 3):
      return '|'
  if(acre == '|'):
    if(count(neighbors, '#') >= 3):
      return '#'
  if(acre == '#'):
    if((count(neighbors, '#') >= 1) and (count(neighbors, '|') >= 1)):
      return '#'
    else:
      return '.'
  return acre


proc resourceScore(board: array[50, array[50, char]]): int =
  var woods = 0
  var lumberyards = 0

  for row in board:
    for acre in row:
      if(acre == '|'):
        woods = woods + 1
      if(acre == '#'):
        lumberyards = lumberyards + 1
  
  return woods * lumberyards



var a_board: array[50, array[50, char]]
var b_board: array[50, array[50, char]]
for i,row in board:
  for j,col in row:
    b_board[i][j] = board[i][j]

var old = b_board
var new = a_board

for minute in 1..1000000000:
  for i,row in board:
    for j,col in row:
      new[i][j] = acreResult(old[i][j], neighbors(i,j,old))
  var t = old
  old = new
  new = t
  if minute %% 1000 == 0:
    echo minute
      
echo resourceScore(old)
