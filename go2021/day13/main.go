package main

import (
	//"bufio"
	"fmt"
	"io/ioutil"
	"log"
	"strings"
)

func getFoldAxis(line string) (string, string) {
	instruction := strings.Split(strings.TrimPrefix(strings.Split(line, "\n")[0], "fold along "), "=")
	axis := instruction[0]
	foldCoord := instruction[1]
	return axis, foldCoord
}

func solvePartOne(data string) (int64, error) {
	dataParts := strings.Split(data, "\n\n")
	coordsAsStrings := strings.Split(dataParts[0], "\n")
	axis, foldCoord := getFoldAxis(dataParts[1])
	var coords []
	for _, line := range coords_str {

	fmt.Println(line)
	}
	fmt.Println(axis, foldCoord)

	return 0, nil
}

func main() {
	fmt.Println("Hello AoC! :* :3")

	//file, err := os.Open("day13/13_test_input")
	file, err := ioutil.ReadFile("day13/13_test_input")
	// pyszzzzzkuuuuuuu
	//    /\___/\
	//   /       \
	//  l  u   u  l
	//--l----*----l--
	//   \   w   /
	//     ======
	//   /       \ __
	//   l        l\ \
	//   l        l/ /
	//   l  l l   l /
	//   \ ml lm /_/
	// :D
	if err != nil {
		log.Fatal(err)
	}
	//scanner := bufio.NewScanner(file)
	//scanner.Split(bufio.ScanLines)
	//var lines []string
	//for scanner.Scan() {
	//	lines = append(lines, scanner.Text())
	//}
	//file.Close()

	result1, err := solvePartOne(string(file))
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(result1)

	//result2, err := solvePartTwo(data)
	//if err != nil {
	//	log.Fatal(err)
	//}
	//fmt.Println(result2)
}
