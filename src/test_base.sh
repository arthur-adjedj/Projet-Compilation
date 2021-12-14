make &&
./pgoc --debug base.go &&
gcc-9 -no-pie base.s -o base &&
./base