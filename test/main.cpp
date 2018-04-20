#include "evaluator1.hpp"
#include "evaluator2.hpp"
#include "evaluator3.hpp"
#include "evaluator4.hpp"
#include "evaluator5.hpp"
#include "evaluator6.hpp"
#include "evaluator7.hpp"
#include "evaluator8.hpp"
#include "evaluator9.hpp"
#include "evaluator10.hpp"
#include "evaluator11.hpp"
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
    case 5: return check(evaluator5(bigVectors), "{50,122}");
    case 6: return check(evaluator6(bigVectors), "{{1,2,3},{2,4,6},{3,6,9}}");
    case 7: return check(evaluator7(bigVectors), "{{22,28},{49,64}}");
    case 8: return check(evaluator8(bigVectors), "{{{50,60},{114,140},{178,220}},{{242,300},{306,380},{370,460}}}");
    case 9: return check(evaluator9(bigVectors), "{4,8,12}");
    case 10: return check(evaluator10(bigVectors), "{3,3,3}");
    case 11: return check(evaluator11(bigVectors), "{4,6,8}");
    default: return 1;
    }
}
