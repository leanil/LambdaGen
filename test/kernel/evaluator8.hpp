#include "View.h"
#include <map>
#include <string>

auto evaluator8(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<4,2>, P<2,1>>> mat8_32(userData["mat8"]);
    View<double const*, double, to_list_t<P<2,12>, P<4,1>, P<3,4>>> tens_33(userData["tens"]);
    View<double*, double, to_list_t<P<2,6>, P<3,2>, P<2,1>>> result;
    View<double*, double, to_list_t<P<3,2>, P<2,1>>> tmp_3;
    
    for (int idx_1 = 0; idx_1 < 2; ++idx_1) {
        [=](auto m2){
            
            
            
            for (int idx_3 = 0; idx_3 < 4; ++idx_3) {
                [=](auto v116, auto v216){
                    
                    
                    
                    for (int idx_17 = 0; idx_17 < 3; ++idx_17) {
                        [=](auto x18){
                            
                            
                            
                            [=](auto x20, auto v20){
                                
                                
                                
                                for (int idx_21 = 0; idx_21 < 2; ++idx_21) {
                                    [=](auto y23){
                                        
                                        auto x23 = x20;
                                        
                                        
                                        tmp_3[idx_17][idx_21] = x23 * y23;
                                    }(v20[idx_21]);
                                }
                            }(x18, v216);
                        }(v116[idx_17]);
                    }
                }(m2[idx_3],mat8_32[idx_3]);
                if(idx_3)
                    [=](auto m14, auto m24){
                        
                        
                        
                        for (int idx_5 = 0; idx_5 < 3; ++idx_5) {
                            [=](auto v16, auto v26){
                                
                                
                                
                                for (int idx_7 = 0; idx_7 < 2; ++idx_7) {
                                    [=](auto x8, auto y8){
                                        
                                        
                                        
                                        
                                        result[idx_1][idx_5][idx_7] = x8 + y8;
                                    }(v16[idx_7],v26[idx_7]);
                                }
                            }(m14[idx_5],m24[idx_5]);
                        }
                    }(result[idx_1],tmp_3);
                else
                    result[idx_1] = tmp_3;
            }
        }(tens_33[idx_1]);
    }
    return result;
}
