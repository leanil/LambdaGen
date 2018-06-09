#include "View.h"
#include <map>
#include <string>

auto evaluator6(std::map<std::string, double*> userData) {
    View<double const*, double, to_list_t<P<3,1>>> a_17(userData["a"]);
    View<double const*, double, to_list_t<P<3,1>>> b_18(userData["b"]);
    View<double*, double, to_list_t<P<3,3>, P<3,1>>> result;
    
    auto v12 = a_17;
    auto v22 = b_18;
    
    
    
    for (int idx_3 = 0; idx_3 < 3; ++idx_3) {
        auto x4 = v12[idx_3];
        
        
        
        auto x6 = x4;
        auto v6 = v22;
        
        
        
        for (int idx_7 = 0; idx_7 < 3; ++idx_7) {
            auto y9 = v6[idx_7];
            
            auto x9 = x6;
            
            
            result[idx_3][idx_7] = x9 * y9;
        }
    }
    return result;
}
