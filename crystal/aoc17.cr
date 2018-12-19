def input
  `cat ../inputs/input-17.0.txt`
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
height = (upper[1] - lower[1]) + 1
width = (upper[0] - lower[0]) + 1
adjusted_points = points.map { |e| {e[0] - lower[0], e[1] - lower[1]} }

puts "h: #{height} w: #{width}"
matrix = Array.new(height) { |i| Array.new(width) { |j| '.' } }

adjusted_points.each do |p|
  puts p
  matrix[p[1]][p[0]] = '#'
end

disp_board matrix
