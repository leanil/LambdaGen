#include "View.h"
#include <map>
#include <string>

auto evaluator16(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<2,3>, P<3,1>>> M1_28(userData["M1"]);
    View<double const*, double, to_list_t<P<3,4>, P<4,1>>> M2_30(userData["M2"]);
    View<double*, double, to_list_t<P<4,2>, P<2,1>>> result;
    View<double*, double, to_list_t<>> tmp_5;
    View<double*, double, to_list_t<>> tmp_11;
    
    auto v_29 = flip<0>(M2_30);
    for (int idx_1 = 0; idx_1 < 4; ++idx_1) {
        [=](auto u2){
            
            
            
            for (int idx_3 = 0; idx_3 < 2; ++idx_3) {
                [=](auto v4){
                    
                    
                    
                    auto v_23 = subdiv<0,1>(u2);
                    auto v_22 = flip<0>(v_23);
                    
                    auto v_26 = subdiv<0,1>(v4);
                    auto v_25 = flip<0>(v_26);
                    for (int idx_5 = 0; idx_5 < 1; ++idx_5) {
                        [=](auto u110, auto u210){
                            
                            
                            
                            for (int idx_11 = 0; idx_11 < 3; ++idx_11) {
                                [=](auto x16, auto y16){
                                    
                                    
                                    
                                    
                                    tmp_11 = x16 * y16;
                                }(u110[idx_11],u210[idx_11]);
                                if(idx_11)
                                    [=](auto x12, auto y12){
                                        
                                        
                                        
                                        
                                        tmp_5 = x12 + y12;
                                    }(tmp_5,tmp_11);
                                else
                                    tmp_5 = tmp_11;
                            }
                        }(v_22[idx_5],v_25[idx_5]);
                        if(idx_5)
                            [=](auto x6, auto y6){
                                
                                
                                
                                
                                result[idx_1][idx_3] = x6 + y6;
                            }(result[idx_1][idx_3],tmp_5);
                        else
                            result[idx_1][idx_3] = tmp_5;
                    }
                }(M1_28[idx_3]);
            }
        }(v_29[idx_1]);
    }
    return result;
}
