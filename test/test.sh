rm -f result*.hpp out.txt eval
runhaskell -i../src ../src/Test.hs
for test in result*.hpp; do
	echo $test
	mv $test result.hpp
	clang++ -I/mnt/hgfs/host/Programs/triSYCL/include/ -I/mnt/hgfs/host/Programs/boost_1_61_0/ -I../src \
		-std=c++14 -pthread main.cpp -o eval
	echo "$test `./eval`" >> out.txt
	mv result.hpp $test
	rm -f eval
done
diff -s out.txt expect.txt
