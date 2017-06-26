#include "helper.h"
#include "my_sycl.h"
#include "View.h"
#include <map>
#include <string>

View<double*,double,2> evaluator(std::map<std::string, double*> bigVectors){
	View<double*,double,2> result(std::array<size_t,1>{1}, new double[2]);
	buffer_t b_vec844138389(const_cast<const double*>(bigVectors.at("vec")),3);
	buffer_t b_mat1838073155(const_cast<const double*>(bigVectors.at("mat")),6);
	buffer_t b_2147482884(result.data, 2);
	buffer_t b_332304331(12);
	cl::sycl::queue deviceQueue;
	deviceQueue.submit([&] (cl::sycl::handler &cgh) {
		act_cgh = &cgh;
		View<accessor,double,3> vec844138389(std::array<size_t,1>{1},b_vec844138389.get_access<rw_access>(cgh));
		View<accessor,double,2,3> mat1838073155(std::array<size_t,2>{3,1},b_mat1838073155.get_access<rw_access>(cgh));
		View<accessor,double,2> v_2147482884(std::array<size_t,1>{1},b_2147482884.get_access<rw_access>(cgh));
		View<accessor,double,4,3> v_332304331(std::array<size_t,2>{3,1},b_332304331.get_access<rw_access>(cgh));
		ParMap([=](View<accessor,double,3> v1){return
		[=](View<accessor,double,3> v2){return
		[=](View<accessor,double> result, unsigned thread_id){
		Zip([=](double x){return
		[=](double y){return
		[=](View<accessor,double> result){
		result=x*y;};};},v1,v2,v_332304331[thread_id]),
		Reduce([=](double x){return
		[=](double y){return
		[=](View<accessor,double> result){
		result=x+y;};};},v_332304331[thread_id],result);};};}(vec844138389),mat1838073155,v_2147482884,4);
	});
	return result;
}
