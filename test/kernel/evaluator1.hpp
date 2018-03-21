#include "helper.h"
#include "my_sycl.h"
#include "View.h"
#include <map>
#include <string>

View<double*,double> evaluator1(std::map<std::string, double*> bigVectors){
	View<double*,double> result(std::array<size_t,0>{}, new double[1]);
	buffer_t b_0(result.data, 1);
	cl::sycl::queue deviceQueue;
	deviceQueue.submit([&] (cl::sycl::handler &cgh) {
		act_cgh = &cgh;
		View<accessor,double> v_0(std::array<size_t,0>{},b_0.get_access<rw_access>(cgh));
		act_cgh->single_task<class SingleKernel>([=] () mutable {
			v_0=20.0;
		});
	});
	return result;
}
