#include "evaluator1.hpp"
#include "evaluator2.hpp"
#include "evaluator3.hpp"
#include "evaluator4.hpp"
#include "evaluator5.hpp"
#include "tester.hpp"
#include <cstdlib>

int main(int argc, char** argv) {
    std::map<std::string, double*> bigVectors{
        { "vec", gen_seq(7,3) },
        { "mat", gen_seq(1,6) },
        { "a", gen_seq(1,3) },
        { "b", gen_seq(1,3) },
        { "mat8", gen_seq(1,8) },
        { "tens", gen_seq(1,24) }
    };
    switch (atoi(argv[1])) {
    case 1: return check(evaluator1(bigVectors), "20");
    case 2: return check(evaluator2(bigVectors), "17");
    case 3: return check(evaluator3(bigVectors), "16");
    case 4: return check(evaluator4(bigVectors), "{50,122}");
    case 5: return check(evaluator5(bigVectors), "{4,8,12}");
    default: return 1;
    }
}
