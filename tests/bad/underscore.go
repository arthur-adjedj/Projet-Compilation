package main

func foo() (int, int) {
	return 1,2
}

func main() {
	var _ = "test"
	var a = 3
	_,a = foo()
	_ = a
}
