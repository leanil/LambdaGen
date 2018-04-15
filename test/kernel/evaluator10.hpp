#include "View.h"
#include <map>
#include <string>

auto evaluator10(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<3,1>>> a(userData["a"]);
    View<double*, double, to_list_t<P<3,1>>> result;
    
    for (int idx_1 = 0; idx_1 < 3; ++idx_1) {
        
        [=](auto x, auto y){
            result[idx_1] = x;
        }(3.0);(a[idx_1]);
    }
    return result;
}
