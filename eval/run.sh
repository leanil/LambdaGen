rm -f result.hpp eval
# echo generating...
runhaskell -i../src ../src/Eval.hs
[ -f result.hpp ] || exit 1
# echo building...
clang++ -I/mnt/hgfs/host/Programs/triSYCL/include/ -I/mnt/hgfs/host/Programs/boost_1_61_0/ -I../src \
		-std=c++14 -pthread main.cpp -o eval
[ -f eval ] || exit 1
# echo running...
./eval
