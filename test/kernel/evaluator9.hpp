#include "View.h"
#include <map>
#include <string>

auto evaluator9(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<3,1>>> a_11(userData["a"]);
    View<double const*, double, to_list_t<P<3,1>>> b_12(userData["b"]);
    View<double*, double, to_list_t<P<3,1>>> result;
    View<double*, double, to_list_t<P<3,1>>> v_6;
    
    for (int idx_6 = 0; idx_6 < 3; ++idx_6) {
        [=](auto x){
            
            
            
            
            v_6[idx_6] = 3.0 * x;
        }(a_11[idx_6]);
    }
    for (int idx_1 = 0; idx_1 < 3; ++idx_1) {
        [=](auto x, auto y){
            
            
            
            
            result[idx_1] = x + y;
        }(v_6[idx_1],b_12[idx_1]);
    }
    return result;
}
