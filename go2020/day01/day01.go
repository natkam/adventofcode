package main

import (
	"errors"
	"fmt"
	"io/ioutil"
	"log"
	"strconv"
	"strings"
)

func solvePartOne(data []string) (int64, error) {
	for i := 0; i < len(data); i++ {
		for j := i + 1; j < len(data); j++ {
			ith, err := strconv.ParseInt(data[i], 10, 64)
			if err != nil {
				log.Fatal(err)
			}
			jth, err := strconv.ParseInt(data[j], 10, 64)
			if err != nil {
				log.Fatal(err)
			}
			if ith+jth == 2020 {
				return ith * jth, nil
			}
		}
	}
	return 0, errors.New("No entries sum up to 2020!")
}

func main() {
	fmt.Println("Hello AoC!")

	file, err := ioutil.ReadFile("day01/01_input")
	if err != nil {
		log.Fatal(err)
	}
	data := strings.Fields(string(file))
	result, err := solvePartOne(data)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(result)
}
