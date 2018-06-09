#include "View.h"
#include <map>
#include <string>

auto evaluator5(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<3,1>>> vec_18(userData["vec"]);
    View<double const*, double, to_list_t<P<3,1>, P<2,3>>> mat_19(userData["mat"]);
    View<double*, double, to_list_t<P<2,1>>> result;
    View<double*, double, to_list_t<P<2,1>>> tmp_1;
    
    for (int idx_1 = 0; idx_1 < 3; ++idx_1) {
        auto x10 = vec_18[idx_1];
        auto v10 = mat_19[idx_1];
        
        
        
        for (int idx_11 = 0; idx_11 < 2; ++idx_11) {
            auto y13 = v10[idx_11];
            
            auto x13 = x10;
            
            
            tmp_1[idx_11] = x13 * y13;
        }
        if(idx_1) {
            auto v12 = result;
            auto v22 = tmp_1;
            
            
            
            for (int idx_3 = 0; idx_3 < 2; ++idx_3) {
                auto x4 = v12[idx_3];
                auto y4 = v22[idx_3];
                
                
                
                
                result[idx_3] = x4 + y4;
            }
        }
        else
            result = tmp_1;
    }
    return result;
}
