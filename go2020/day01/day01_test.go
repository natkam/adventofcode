package main

import "testing"

func TestSolvePartOne(t *testing.T) {
	input := []string{"1721", "979", "366", "299", "675", "1456"}
	var want int64 = 514579
	result, err := solvePartOne(input)
	if want != result || err != nil {
		t.Fatalf(`solvePartOne(input) = %v, %v; want %#q`, result, err, want)
	}
}


func TestSolvePartOneWithoutResult(t *testing.T) {
	input := []string{"1720", "979", "366", "299", "675", "1456"}
	result, err := solvePartOne(input)
	if result != 0 || err == nil {
		t.Fatalf(`solvePartOne(input) = %v, %v; want 0`, result, err)
	}
}
