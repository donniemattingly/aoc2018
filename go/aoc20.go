package main

import "fmt"
import "io/ioutil"
import "strings"

func main() {

	b, err := ioutil.ReadFile("../inputs/input-20.1.txt") // just pass the file name
	if err != nil {
		fmt.Print(err)
	}
	var input = strings.Trim(string(b), " \n")

	var size = partOne(input)
	fmt.Println(size)
}

/* stack implementation from:
	https://stackoverflow.com/questions/28541609/looking-for-reasonable-stack-implementation-in-golang
 */

type Path struct {
	x, y, dist int
}

type Point struct {
	x, y interface{}
}

type stack []Path

func (s stack) Push(v Path) stack {
	return append(s, v)
}

func (s stack) Pop() (stack, Path) {
	// FIXME: What do we do if the stack is empty, though?

	l := len(s)
	return s[:l-1], s[l-1]
}

func (s stack) TakeLast() (stack, Path) {
	return s[1:], s[0]
}

func partOne(s string) int {
	var moves stack
	var distances = make(map[Point]int)
	var dist = 0
	var p Path
	var x = 0
	var y = 0
	for _, char := range s {
		if char == '(' {
			moves = moves.Push(Path{x, y, dist})
		} else if char == ')' {
			moves, p = moves.Pop()
			x = p.x
			y = p.y
			dist = p.dist
		} else if char == '|' {
			p = moves[len(moves) - 1]
			x = p.x
			y = p.y
			dist = p.dist
		} else {
			if char == 'E' {
				x++
			} else if char == 'W' {
				x--
			} else if char == 'N' {
				y++
			} else if char == 'S' {
				y --
			}
			dist++

			var curPoint = Point{x,y}
			var cur = distances[curPoint]
			if cur == 0 || dist < cur {
				distances[curPoint] = dist
			}
		}
	}

	var maxDist = 0
	for _, dist := range distances {
		if dist > maxDist {
			maxDist = dist
		}
	}

	return maxDist
}

func splitTopLevel(s string) []string {
	var result []string
	var paren_depth = 0
	var last_split = 0
	for pos, char := range s {
		if char == '(' {
			paren_depth++
		} else if char == ')' {
			paren_depth--
		} else if char == '|' && paren_depth == 0 {
			result = append(result, string(s[last_split:pos]))
			last_split = pos + 1
		}
	}
	result = append(result, string(s[last_split:]))
	// fmt.Println("----------")
	// fmt.Println(s)
	// fmt.Println(result)
	return result
}

func foo(s string) int {
	//fmt.Println(s)
	var base []string
	var options []string
	var parenDepth = 0
	var lastSplit = 0
	for pos, char := range s {
		if char == '(' {
			if parenDepth == 0 && lastSplit != pos {
				//fmt.Printf("%s ls: %d size:%d pos:%d \n",s, lastSplit, len(s), pos)
				base = append(base, string(s[lastSplit:pos]))
				lastSplit = pos + 1
			}
			parenDepth++
		} else if char == ')' {
			parenDepth--
			if parenDepth == 0 {
				var moreOpts = splitTopLevel(string(s[lastSplit:pos]))
				options = append(options, moreOpts...)
				lastSplit = pos + 1
			}
		}
	}

	if lastSplit < len(s) {
		var newS = string(s[lastSplit:])
		base = append(base, newS)
	}

	//fmt.Println("opt:")
	//fmt.Println(options)
	var size = 0
	for _, val := range options {
		var curSize = foo(val)
		//fmt.Printf("val: %s\n", val)
		if curSize > size {
			size = curSize
		}
	}

	//fmt.Println("base:")
	//fmt.Println(base)
	var total = size
	for _, val := range base {
		//fmt.Printf("base: %s\n", val)
		total = total + len(val)
	}
	// fmt.Println("----------")
	// fmt.Println(s)
	// fmt.Println(result)

	fmt.Printf("size: %d for %s\n", total, s)
	return total
}
