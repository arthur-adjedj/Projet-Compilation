package main

type A struct { a *A }

func test() *A {return new(A)}

func main() {
    x := new(A)
    x = x
}
