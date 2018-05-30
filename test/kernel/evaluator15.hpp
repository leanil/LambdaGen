#include "View.h"
#include <map>
#include <string>

auto evaluator15(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<2,3>, P<3,1>>> M1_18(userData["M1"]);
    View<double const*, double, to_list_t<P<3,4>, P<4,1>>> M2_20(userData["M2"]);
    View<double*, double, to_list_t<P<4,2>, P<2,1>>> result;
    View<double*, double, to_list_t<>> tmp_5;
    
    auto v_19 = flip<0>(M2_20);
    for (int idx_1 = 0; idx_1 < 4; ++idx_1) {
        [=](auto u2){
            
            
            
            auto v_17 = flip<0>(M1_18);
            auto v_16 = flip<0>(v_17);
            for (int idx_3 = 0; idx_3 < 2; ++idx_3) {
                [=](auto v4){
                    
                    
                    
                    for (int idx_5 = 0; idx_5 < 3; ++idx_5) {
                        [=](auto x10, auto y10){
                            
                            
                            
                            
                            tmp_5 = x10 * y10;
                        }(u2[idx_5],v4[idx_5]);
                        if(idx_5)
                            [=](auto x6, auto y6){
                                
                                
                                
                                
                                result[idx_1][idx_3] = x6 + y6;
                            }(result[idx_1][idx_3],tmp_5);
                        else
                            result[idx_1][idx_3] = tmp_5;
                    }
                }(v_16[idx_3]);
            }
        }(v_19[idx_1]);
    }
    return result;
}
