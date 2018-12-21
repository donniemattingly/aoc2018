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
	var size = longestOptionSize(input)
	fmt.Println(size)
}

var option = regexp.MustCompile(`\(.*\)`)
func longestOptionSize(x string) int{
	var result = option.FindString(x)

	if(result == ""){
		return len(x)
	}

	var base = len(x) - len(result)
	var stripped = result[1:len(result) - 1]
	fmt.Println(stripped)
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
