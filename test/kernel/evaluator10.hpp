#include "View.h"
#include <map>
#include <string>

auto evaluator10(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<3,1>>> a_5(userData["a"]);
    View<double*, double, to_list_t<P<3,1>>> result;
    
    for (int idx_1 = 0; idx_1 < 3; ++idx_1) {
        [=](auto y3){
            
            auto x3 = 3.0;
            result[idx_1] = x3;
        }(a_5[idx_1]);
    }
    return result;
}
