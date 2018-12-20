def input
  `cat ../inputs/input-17.1.txt`
    .split("\n")
    .reject(&.empty?)
    .compact
    .flat_map { |line| parse_line(line) }
end

def parse_line(line : String)
  x, y = line.split(", ").sort.map { |x| x[2..-1] }
  xs = [] of Int32
  ys = [] of Int32

  if x.includes? "."
    if md = x.match(/(\d+)..(\d+)/)
      xb = md.captures.compact.map(&.to_i)
      if xb != Nil
        xs = (xb[0]..xb[1]).to_a
        ys = [y.to_i] * xs.size
      end
    end
  else
    if md = y.match(/(\d+)..(\d+)/)
      yb = md.captures.compact.map(&.to_i)
      if yb != Nil
        ys = (yb[0]..yb[1]).to_a
        xs = [x.to_i] * ys.size
      end
    end
  end

  xs.zip(ys)
end

def bounds(points)
  x = points.minmax_by { |p| p[0] }
  y = points.minmax_by { |p| p[1] }

  [{x[0][0], y[0][1]}, {x[1][0], y[1][1]}]
end

def disp_board(board)
  puts board.map { |r| r.join("") }.join("\n")
end

points = input()
lower, upper = bounds(points)
height = (upper[1] - lower[1]) + 2
width = (upper[0] - lower[0]) + 2
adjusted_points = points.map { |e| {e[0] - (lower[0] - 1), e[1] - lower[1]} }

# puts "h: #{height} w: #{width}"
matrix = Array.new(height) { |i| Array.new(width) { |j| '.' } }

adjusted_points.each do |p|
  matrix[p[1]][p[0]] = '#'
end

# rules for water spreading to adjacent cells
# assumption is that point has water
def fill(board, point, height)
  x, y = point
  if (y >= height - 2)
    return
  end

  # below is sand
  if (board[y + 1][x] == '.')
    board[y + 1][x] = '|'
    fill(board, {x, y + 1}, height)
  end

  # below is settled water or clay
  if (board[y + 1][x] == '#' || board[y + 1][x] == '~')
    # sand on left
    if (board[y][x - 1] == '.')
      board[y][x - 1] = '|'
      fill(board, {x - 1, y}, height)
    end

    # sand on right
    if (board[y][x + 1] == '.')
      board[y][x + 1] = '|'
      fill(board, {x + 1, y}, height)
    end

    # check for reservoirs
    left = '0'
    lOffset = 1
    right = '0'
    rOffset = 1

    while left == '0'
      c = board[y][x - lOffset]
      if c == '#' || c == '.'
        left = c
        break
      end
      lOffset += 1
    end

    while (right == '0')
      c = board[y][x + rOffset]
      if c == '#' || c == '.'
        right = c
        break
      end
      rOffset += 1
    end

    # puts "#{right}, #{left}"

    if (left == '#' && right == '#')
      ((x - (lOffset - 1))..(x + (rOffset - 1))).each { |e| board[y][e] = '~' }
    end
  end
end

fill(matrix, {500 - lower[0], 0}, height)

puts matrix.flatten.reject { |x| x == '.' || x == '#' }.size + 1

puts matrix.flatten.select { |x| x == '~' }.size
