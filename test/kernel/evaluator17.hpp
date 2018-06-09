#include "View.h"
#include <map>
#include <string>

auto evaluator17(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<2,3>, P<3,1>>> M1_26(userData["M1"]);
    View<double const*, double, to_list_t<P<3,4>, P<4,1>>> M2_28(userData["M2"]);
    View<double*, double, to_list_t<P<4,2>, P<2,1>>> result;
    View<double*, double, to_list_t<>> tmp_5;
    View<double*, double, to_list_t<>> tmp_11;
    
    auto v_27 = flip<0>(M2_28);
    for (int idx_1 = 0; idx_1 < 4; ++idx_1) {
        auto u2 = v_27[idx_1];
        
        
        
        for (int idx_3 = 0; idx_3 < 2; ++idx_3) {
            auto v4 = M1_26[idx_3];
            
            
            
            auto v_22 = subdiv<0,1>(u2);
            
            auto v_24 = subdiv<0,1>(v4);
            for (int idx_5 = 0; idx_5 < 3; ++idx_5) {
                auto u110 = v_22[idx_5];
                auto u210 = v_24[idx_5];
                
                
                
                for (int idx_11 = 0; idx_11 < 1; ++idx_11) {
                    auto x16 = u110[idx_11];
                    auto y16 = u210[idx_11];
                    
                    
                    
                    
                    tmp_11 = x16 * y16;
                    if(idx_11) {
                        auto x12 = tmp_5;
                        auto y12 = tmp_11;
                        
                        
                        
                        
                        tmp_5 = x12 + y12;
                    }
                    else
                        tmp_5 = tmp_11;
                }
                if(idx_5) {
                    auto x6 = result[idx_1][idx_3];
                    auto y6 = tmp_5;
                    
                    
                    
                    
                    result[idx_1][idx_3] = x6 + y6;
                }
                else
                    result[idx_1][idx_3] = tmp_5;
            }
        }
    }
    return result;
}
