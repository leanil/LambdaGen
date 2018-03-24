#include "helper.h"
#include "my_sycl.h"
#include "View.h"
#include <map>
#include <string>

View<double*,double,2,2> evaluator7(std::map<std::string, double*> bigVectors){
	View<double*,double,2,2> result(std::array<size_t,2>{2,1}, new double[4]);
	buffer_t b_mat898856380(const_cast<const double*>(bigVectors.at("mat")),6);
	buffer_t b_mat931609811(const_cast<const double*>(bigVectors.at("mat")),6);
	buffer_t b_2147482884(result.data, 4);
	buffer_t b_1675647699(3);
	cl::sycl::queue deviceQueue;
	deviceQueue.submit([&] (cl::sycl::handler &cgh) {
		act_cgh = &cgh;
		View<accessor,double,2,3> mat898856380(std::array<size_t,2>{3,1},b_mat898856380.get_access<rw_access>(cgh));
		View<accessor,double,2,3> mat931609811(std::array<size_t,2>{1,2},b_mat931609811.get_access<rw_access>(cgh));
		View<accessor,double,2,2> v_2147482884(std::array<size_t,2>{2,1},b_2147482884.get_access<rw_access>(cgh));
		View<accessor,double,3> v_1675647699(std::array<size_t,1>{1},b_1675647699.get_access<rw_access>(cgh));
		act_cgh->single_task<class SingleKernel>([=] () mutable {
			[=](auto m1){return
			[=](auto m2){return
			[=](auto result){return
			Map([=](auto v){return
			[=](auto result){return
			Map([=](auto v1){return
			[=](auto v2){return
			[=](auto result){return
			Zip([=](auto x){return
			[=](auto y){return
			[=](auto result){return
			result=x*y;};};},v1,v2,v_1675647699),
			Reduce([=](auto x){return
			[=](auto y){return
			[=](auto result){return
			result=x+y;};};},v_1675647699,result);};};}(v),m2,result);};},m1,result);};};}(mat898856380)(mat931609811)(v_2147482884);
		});
	});
	return result;
}
