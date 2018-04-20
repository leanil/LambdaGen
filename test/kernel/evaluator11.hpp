#include "View.h"
#include <map>
#include <string>

auto evaluator11(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<3,1>>> a_12(userData["a"]);
    View<double*, double, to_list_t<P<3,1>>> result;
    View<double*, double, to_list_t<P<3,1>>> v_7;
    
    for (int idx_7 = 0; idx_7 < 3; ++idx_7) {
        [=](auto x){
            
            
            
            
            v_7[idx_7] = x + 1.0;
        }(a_12[idx_7]);
    }
    for (int idx_1 = 0; idx_1 < 3; ++idx_1) {
        [=](auto y){
            
            auto x1 = 2.0;
            
            
            result[idx_1] = x1 * y;
        }(v_7[idx_1]);
    }
    return result;
}
