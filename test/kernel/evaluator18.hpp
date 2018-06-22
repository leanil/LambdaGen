#include "View.h"
#include <map>
#include <string>

auto evaluator18(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<2,3>, P<3,1>>> M1_19(userData["M1"]);
    View<double const*, double, to_list_t<P<3,4>, P<4,1>>> M2_23(userData["M2"]);
    View<double*, double, to_list_t<P<4,2>, P<2,1>>> result;
    View<double*, double, to_list_t<P<4,2>, P<1,2>, P<2,1>>> v_2;
    View<double*, double, to_list_t<>> tmp_8;
    
    auto v_22 = flip<0>(M2_23);
    auto v_21 = subdiv<0,1>(v_22);
    for (int idx_2 = 0; idx_2 < 4; ++idx_2) {
        auto u13 = v_21[idx_2];
        
        
        
        for (int idx_4 = 0; idx_4 < 1; ++idx_4) {
            auto u5 = u13[idx_4];
            
            
            
            for (int idx_6 = 0; idx_6 < 2; ++idx_6) {
                auto v7 = M1_19[idx_6];
                
                
                
                for (int idx_8 = 0; idx_8 < 3; ++idx_8) {
                    auto x13 = u5[idx_8];
                    auto y13 = v7[idx_8];
                    
                    
                    
                    
                    tmp_8 = x13 * y13;
                    if(idx_8) {
                        auto x9 = v_2[idx_2][idx_4][idx_6];
                        auto y9 = tmp_8;
                        
                        
                        
                        
                        v_2[idx_2][idx_4][idx_6] = x9 + y9;
                    }
                    else
                        v_2[idx_2][idx_4][idx_6] = tmp_8;
                }
            }
        }
    }
    result = flatten<0>(v_2);
    return result;
}
