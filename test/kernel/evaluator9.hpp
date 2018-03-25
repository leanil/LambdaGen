#include "helper.h"
#include "my_sycl.h"
#include "View.h"
#include <map>
#include <string>

View<double*,double,3> evaluator9(std::map<std::string, double*> bigVectors){
	View<double*,double,3> result(std::array<size_t,1>{1}, new double[3]);
	buffer_t b_a162533727(const_cast<const double*>(bigVectors.at("a")),3);
	buffer_t b_b931609811(const_cast<const double*>(bigVectors.at("b")),3);
	buffer_t b_2147482884(result.data, 3);
	buffer_t b_898856380(3);
	cl::sycl::queue deviceQueue;
	deviceQueue.submit([&] (cl::sycl::handler &cgh) {
		act_cgh = &cgh;
		View<accessor,double,3> a162533727(std::array<size_t,1>{1},b_a162533727.get_access<rw_access>(cgh));
		View<accessor,double,3> v_898856380(std::array<size_t,1>{1},b_898856380.get_access<rw_access>(cgh));
		ParMap([=](auto x){return
		[=](auto result, unsigned thread_id){
		result=3.0*x;};},a162533727,v_898856380,4);
	});
	deviceQueue.submit([&] (cl::sycl::handler &cgh) {
		act_cgh = &cgh;
		View<accessor,double,3> a162533727(std::array<size_t,1>{1},b_a162533727.get_access<rw_access>(cgh));
		View<accessor,double,3> b931609811(std::array<size_t,1>{1},b_b931609811.get_access<rw_access>(cgh));
		View<accessor,double,3> v_2147482884(std::array<size_t,1>{1},b_2147482884.get_access<rw_access>(cgh));
		View<accessor,double,3> v_898856380(std::array<size_t,1>{1},b_898856380.get_access<rw_access>(cgh));
		ParZip([=](auto x){return
		[=](auto y){return
		[=](auto result, unsigned thread_id){
		result=x+y;};};},v_898856380,b931609811,v_2147482884,4);
	});
	return result;
}
