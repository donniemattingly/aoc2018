function read_input(filename)
	fileString = open(filename) do file
    	read(file, String)
	end

	lines = split(fileString, "\n")
	coords = map(y -> map(x -> parse(Int, replace(x, ","=> "")), split(y)), lines)

	return coords
end

function get_max_size(coords)
	x_max = maximum([ x[1] for x in coords])
	y_max = maximum([ x[2] for x in coords])
	return (x_max, y_max)
end

function manhattan_distance_between(c1, c2)
	return abs(c1[1] - c2[1]) + abs(c1[2] - c2[2])
end

function closest_coordinate(c, coords)
	closest_coord_index = -1
	dist = typemax(Int)
	coords_with_dist = 0
	for (index, coord) in enumerate(coords)
		cur_dist = manhattan_distance_between(c, coord)
		if cur_dist == dist
			coords_with_dist = coords_with_dist + 1
		elseif cur_dist < dist
			dist = cur_dist
			coords_with_dist = 0
			closest_coord_index = index
		end
	end

	if coords_with_dist > 0
		return -1
	else
		return closest_coord_index
	end
end

function calculated_distances(w, h, coords)
	array = zeros(Int, w, h)

	for i in range(1, stop=w)
		for j in range(1, stop=h)
			dist = closest_coordinate([i, j], coords)
			array[i, j] = dist
		end
	end

	return array
end

function get_infinite_regions(d)
	(x, y) = size(d)
	inf_coords = [d[:, 1] ; d[:, y]; d[1, :]; d[x, :]; [-1]]
	return Set(inf_coords)
end

function get_region_sizes(d)
	sizes = Dict()
	for i in d
		sizes[i] = get(sizes, i, 0) + 1
	end

	return sizes
end

function get_largest_valid_region_size(region_sizes, infinite_regions)
	 return filter!(x -> !in(x[1], infinite_regions), sort(collect(region_sizes), by = tuple -> last(tuple), rev=true))
end

function dist_from_all_coords(c, coords)
	sum(map(x -> manhattan_distance_between(c, x), coords))
end

function calculated_distances_from_all(w,h,coords, thres)
	array = zeros(Int, w, h)

	for i in range(1, stop=w)
		for j in range(1, stop=h)
			dist = dist_from_all_coords([i, j], coords)
			if dist < thres
				array[i, j] = 1
			end
		end
	end

	return array
end

function part_one(input)
	(x_max, y_max) = get_max_size(input)
	dist_array = calculated_distances(x_max, y_max, input)
	infinite_regions = get_infinite_regions(dist_array)
	region_sizes = get_region_sizes(dist_array)
	largest_region = get_largest_valid_region_size(region_sizes, infinite_regions)
	return largest_region[1]
end

function part_two(input)
	(x_max, y_max) = get_max_size(input)
	dist_array = calculated_distances_from_all(x_max, y_max, input, 10000)
	return sum(dist_array)
end


test_input = read_input("../inputs/input-6.0.txt")
input = read_input("../inputs/input-6.1.txt")


