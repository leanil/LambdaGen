#include "View.h"
#include <map>
#include <string>

auto evaluator14(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<2,3>, P<3,1>>> M1_21(userData["M1"]);
    View<double const*, double, to_list_t<P<3,4>, P<4,1>>> M2_23(userData["M2"]);
    View<double*, double, to_list_t<P<4,2>, P<2,1>>> result;
    View<double*, double, to_list_t<P<2,1>>> tmp_3;
    
    auto v_22 = flip<0>(M2_23);
    for (int idx_1 = 0; idx_1 < 4; ++idx_1) {
        [=](auto u2){
            
            
            
            auto v_20 = flip<0>(M1_21);
            for (int idx_3 = 0; idx_3 < 3; ++idx_3) {
                [=](auto x12, auto v12){
                    
                    
                    
                    for (int idx_13 = 0; idx_13 < 2; ++idx_13) {
                        [=](auto y14){
                            
                            
                            
                            
                            tmp_3[idx_13] = x12 * y14;
                        }(v12[idx_13]);
                    }
                }(u2[idx_3],v_20[idx_3]);
                if(idx_3)
                    [=](auto u14, auto u24){
                        
                        
                        
                        for (int idx_5 = 0; idx_5 < 2; ++idx_5) {
                            [=](auto x6, auto y6){
                                
                                
                                
                                
                                result[idx_1][idx_5] = x6 + y6;
                            }(u14[idx_5],u24[idx_5]);
                        }
                    }(result[idx_1],tmp_3);
                else
                    result[idx_1] = tmp_3;
            }
        }(v_22[idx_1]);
    }
    return result;
}
