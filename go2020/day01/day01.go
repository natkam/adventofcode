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
	return 0, errors.New("no two entries sum up to 2020")
}

func solvePartTwo(data []string) (int64, error) {
	for i := 0; i < len(data); i++ {
		for j := i + 1; j < len(data); j++ {
			for k := j + 1; k < len(data); k++ {
				ith, err := strconv.ParseInt(data[i], 10, 64)
				if err != nil {
					log.Fatal(err)
				}
				jth, err := strconv.ParseInt(data[j], 10, 64)
				if err != nil {
					log.Fatal(err)
				}
				kth, err := strconv.ParseInt(data[k], 10, 64)
				if err != nil {
					log.Fatal(err)
				}

				if ith+jth+kth == 2020 {
					return ith * jth * kth, nil
				}
			}
		}
	}
	return 0, errors.New("no three entries sum up to 2020")
}

func main() {
	fmt.Println("Hello AoC!")

	file, err := ioutil.ReadFile("day01/01_input")
	if err != nil {
		log.Fatal(err)
	}
	data := strings.Fields(string(file))

	result1, err := solvePartOne(data)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(result1)

	result2, err := solvePartTwo(data)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(result2)
}
