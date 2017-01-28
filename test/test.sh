runhaskell -i.. ../Test.hs
rm -f result.hpp out.txt
for test in result*.hpp; do
	echo $test
	mv $test result.hpp
	./build.sh
	echo "$test `./eval`" >> out.txt
	mv result.hpp $test
done
diff -s out.txt expect.txt
