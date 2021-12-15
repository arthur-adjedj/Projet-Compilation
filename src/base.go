package main

import "fmt"

type L struct {
	x    int
	next int
}

func foo (x int) int {
	return x
}

func main() {
	z := new(L)
	fmt.Print(*z)

}
