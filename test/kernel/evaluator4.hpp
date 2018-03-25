#include "helper.h"
#include "my_sycl.h"
#include "View.h"
#include <map>
#include <string>

View<double*,double,2> evaluator4(std::map<std::string, double*> bigVectors){
	View<double*,double,2> result(std::array<size_t,1>{1}, new double[2]);
	buffer_t b_vec1876612334(const_cast<const double*>(bigVectors.at("vec")),3);
	buffer_t b_mat1838073155(const_cast<const double*>(bigVectors.at("mat")),6);
	buffer_t b_2147482884(result.data, 2);
	buffer_t b_1830426177(12);
	cl::sycl::queue deviceQueue;
	deviceQueue.submit([&] (cl::sycl::handler &cgh) {
		act_cgh = &cgh;
		View<accessor,double,3> vec1876612334(std::array<size_t,1>{1},b_vec1876612334.get_access<rw_access>(cgh));
		View<accessor,double,2,3> mat1838073155(std::array<size_t,2>{3,1},b_mat1838073155.get_access<rw_access>(cgh));
		View<accessor,double,2> v_2147482884(std::array<size_t,1>{1},b_2147482884.get_access<rw_access>(cgh));
		View<accessor,double,4,3> v_1830426177(std::array<size_t,2>{3,1},b_1830426177.get_access<rw_access>(cgh));
		ParMap([=](auto v2){return
		[=](auto result, unsigned thread_id){
		auto v1=vec1876612334;
		Zip([=](auto x){return
		[=](auto y){return
		[=](auto result){
		result=x*y;};};},v1,v2,v_1830426177[thread_id]),
		Reduce([=](auto x){return
		[=](auto y){return
		[=](auto result){
		result=x+y;};};},v_1830426177[thread_id],result);};},mat1838073155,v_2147482884,4);
	});
	return result;
}
