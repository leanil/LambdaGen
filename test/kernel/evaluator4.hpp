#include "View.h"
#include <map>
#include <string>

auto evaluator4(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<3,1>>> vec_4(userData["vec"]);
    View<double const*, double, to_list_t<P<2,3>, P<3,1>>> mat_17(userData["mat"]);
    View<double*, double, to_list_t<P<2,1>>> result;
    View<double*, double, to_list_t<P<3,1>>> v_10;
    
    for (int idx_1 = 0; idx_1 < 2; ++idx_1) {
        [=](auto v2){
            
            auto v1 = vec_4;
            
            
            for (int idx_10 = 0; idx_10 < 3; ++idx_10) {
                [=](auto x, auto y){
                    
                    
                    v_10[idx_10] = x * y;
                }(v1[idx_10],v2[idx_10]);
            }
            result[idx_1] = v_10[0];
            for (int idx_5 = 1; idx_5 < 3; ++idx_5) {
                [=](auto x, auto y){
                    
                    
                    result[idx_1] = x + y;
                }(result[idx_1],v_10[idx_5]);
            }
        }(mat_17[idx_1]);
    }
    return result;
}
