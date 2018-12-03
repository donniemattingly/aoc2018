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

containsDuplicates =: 3 : 0
	(# y) ~: (# ~. y)
)

firstDuplicate =: 3 : 0
	duplicatesTest =. (containsDuplicates \ y)
	firstDuplicateIndex =. duplicatesTest i. 1
	try.
	result =. firstDuplicateIndex { y 
	catch.
	0
	end.
)

part2 =: verb define
 input =. inputAsList input1
 a=. ,
 b=. 0
 count =. 0
 while. (b = 0) *. (count < 250) do.
 	a =. a , input
 	count =. count + 1
 	b = firstDuplicate (+/ \ a)
 end. 
 b
)

NB. Solved this originally:
NB. firstDuplicate (+/ \ (data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data, data ))