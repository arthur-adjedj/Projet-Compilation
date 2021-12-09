make &&
./pgoc base.go &&
gcc -no-pie base.s -o base &&
./base