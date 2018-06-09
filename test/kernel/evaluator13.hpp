#include "View.h"
#include <map>
#include <string>

auto evaluator13(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<3,4>, P<4,1>>> M2_18(userData["M2"]);
    View<double const*, double, to_list_t<P<2,3>, P<3,1>>> M1_19(userData["M1"]);
    View<double*, double, to_list_t<P<4,1>, P<2,4>>> result;
    View<double*, double, to_list_t<P<2,4>, P<4,1>>> v_2;
    View<double*, double, to_list_t<>> tmp_6;
    
    for (int idx_2 = 0; idx_2 < 2; ++idx_2) {
        auto v3 = M1_19[idx_2];
        
        
        
        auto v_17 = flip<0>(M2_18);
        for (int idx_4 = 0; idx_4 < 4; ++idx_4) {
            auto u5 = v_17[idx_4];
            
            
            
            for (int idx_6 = 0; idx_6 < 3; ++idx_6) {
                auto x11 = u5[idx_6];
                auto y11 = v3[idx_6];
                
                
                
                
                tmp_6 = x11 * y11;
                if(idx_6) {
                    auto x7 = v_2[idx_2][idx_4];
                    auto y7 = tmp_6;
                    
                    
                    
                    
                    v_2[idx_2][idx_4] = x7 + y7;
                }
                else
                    v_2[idx_2][idx_4] = tmp_6;
            }
        }
    }
    result = flip<0>(v_2);
    return result;
}
