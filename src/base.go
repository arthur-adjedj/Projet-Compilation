package main
import "fmt"

type L struct {
	x    int
	next *L
}


func main() {
	a := new(L)
	a.x = 12
	z := new(L)
	z.x = 2
	z.next = a 
	fmt.Print(*z,*a)

}
