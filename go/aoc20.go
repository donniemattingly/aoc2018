package main

import "fmt"
import "regexp"
import "io/ioutil"
import "strings"

func main() {

	b, err := ioutil.ReadFile("../inputs/input-20.1.txt") // just pass the file name
	if err != nil {
		fmt.Print(err)
	}
	var input = strings.Trim(string(b), " \n")
	fmt.Println(input)
	var size = foo(input)
	fmt.Println(size)
}

var option = regexp.MustCompile(`\(.*\)`)

/*

 */

func tokenizeString(x string) []string {
	var paren_depth = 0
	var result []string
	var group_start = 0
	for pos, char := range x {
		if char == '(' {
			paren_depth++
			group_start = pos
		} else if char == ')' {
			paren_depth--
			if(paren_depth == 0){
				result = append(result, string(x[group_start:pos]))
			}
		}
	}

	return result
}
func longestOptionSize(x string) int{
	//var result = option.FindString(x)

	var paren_depth = 0
	var result []string
	var group_start = 0
	for pos, char := range x {
		if char == '(' {
			paren_depth++
			group_start = pos
		} else if char == ')' {
			paren_depth--
			if(paren_depth == 0){
				result = append(result, string(x[group_start:pos]))
			}
		}
	}

	if(result == ""){
		return len(x)
	}

	var base = len(x) - len(result)


	var stripped = result[1:len(result) - 1]
	//fmt.Println(stripped)
	var max = 0
	var split = splitTopLevel(stripped)

	for _, elem := range split {
		var size = longestOptionSize(elem)
		if size > max{
			max = size
		}
	}

	return base + max
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
	result = append(result, string(s[last_split:len(s)]))
	// fmt.Println("----------")
	// fmt.Println(s)
	// fmt.Println(result)
	return result
}

func foo(s string) int {
	var base []string
	var options []string
	var paren_depth = 0
	var last_split = 0
	for pos, char := range s {
		if char == '(' {
			if paren_depth == 0 {
				base = append(base, string(s[last_split:pos-1]))
				last_split = pos + 1
			}
			paren_depth++
		} else if char == ')' {
			paren_depth--
			if paren_depth == 0 {
				options = append(options, string(s[last_split:pos]))
				last_split = pos + 1
			}
		} else if char == '|' && paren_depth == 0 {
			options = append(options, string(s[last_split:pos]))
			last_split = pos + 1
		}
	}

	base = append(base, string(s[last_split]))
	var size = 0
	for _, val := range options {
		var cur_size = foo(val)
		if cur_size > size {
			size = cur_size
		}
	}

	var total = len(base) + size
	// fmt.Println("----------")
	// fmt.Println(s)
	// fmt.Println(result)
	return total
}
