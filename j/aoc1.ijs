input1 =: '../inputs/input-1.1.txt'

inputAsList =: 3 : 0
	filename =. < y
	raw_data =. 1!:1 filename
	converted =. toJ raw_data
	newline_stripped =. cutopen converted
	parsed =. 0 ". each newline_stripped
	list =. > parsed
)

part1 =: 3 : 0	
	+/ y
)

