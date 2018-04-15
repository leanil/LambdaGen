#include "View.h"
#include <map>
#include <string>

auto evaluator11(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<3,1>>> a(userData["a"]);
    View<double*, double, to_list_t<P<3,1>>> result;
    View<double*, double, to_list_t<P<3,1>>> v_8;
    
    for (int idx_8 = 0; idx_8 < 3; ++idx_8) {
        [=](auto x){
            
            
            v_8[idx_8] = x + 1.0;
        }(a[idx_8]);
    }
    for (int idx_1 = 0; idx_1 < 3; ++idx_1) {
        
        [=](auto x, auto y){
            
            
            result[idx_1] = x * y;
        }(2.0);(v_8[idx_1]);
    }
    return result;
}
