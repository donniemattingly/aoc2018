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


proc printBoard(board: seq[seq[char]]) =
  var output = ""
  for line in board:
    var rowStr = ""
    for chr in line:
      rowStr = rowStr & chr
    output = output & rowStr & "\n"
  echo output

proc neighbors(x: int, y: int, b: seq[seq[char]]): seq[char] =
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


proc resourceScore(board: seq[seq[char]]): int =
  var woods = 0
  var lumberyards = 0

  for row in board:
    for acre in row:
      if(acre == '|'):
        woods = woods + 1
      if(acre == '#'):
        lumberyards = lumberyards + 1
  
  return woods * lumberyards

printBoard(board)

var temp_board: seq[seq[char]] = @[]

var scores = {-1 : -1}.newTable

for minute in 1..1000:
  var new_board: seq[seq[char]] = @[]
  for i,row in board:
    var new_row: seq[char] = @[]
    for j,col in row:
      var cur = board[i][j]
      new_row.add(acreResult(cur, neighbors(i,j,board)))
    new_board.add(new_row)
  board = new_board
  var score = resourceScore(board)
  
  if hasKey(scores, score):
    echo fmt"Repeat at {minute} diff: {minute - scores[score]} score: {score}"
  else:
    scores[score] = minute

  # if minute %% 1000 == 0:
    # echo fmt"After {minute} minutes:"
    # echo resourceScore(board)
      
echo resourceScore(board)
