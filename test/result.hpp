#include "helper.h"
#include "my_sycl.h"
#include "View.h"
#include <map>
#include <string>

View<double*,double,2,3,2> evaluator(std::map<std::string, double*> bigVectors){
	View<double*,double,2,3,2> result(std::array<size_t,3>{6,2,1}, new double[12]);
	buffer_t b_mat8(const_cast<const double*>(bigVectors.at("mat8")),8);
	buffer_t b_tens(const_cast<const double*>(bigVectors.at("tens")),24);
	buffer_t b_2147482884(result.data, 12);
	buffer_t b_2061724296(192);
	cl::sycl::queue deviceQueue((cl::sycl::gpu_selector()));
	deviceQueue.submit([&] (cl::sycl::handler &cgh) {
		act_cgh = &cgh;
		View<accessor,double,4,2> v_mat8(std::array<size_t,2>{2,1},b_mat8.get_access<rw_access>(cgh));
		View<accessor,double,2,4,3> v_tens(std::array<size_t,3>{12,1,4},b_tens.get_access<rw_access>(cgh));
		View<accessor,double,2,3,2> v_2147482884(std::array<size_t,3>{6,2,1},b_2147482884.get_access<rw_access>(cgh));
		View<accessor,double,8,4,3,2> v_2061724296(std::array<size_t,4>{24,6,2,1},b_2061724296.get_access<rw_access>(cgh));
		ParMap([=](View<accessor,double,4,3> m){return
		[=](View<accessor,double,3,2> result, unsigned thread_id){
		Zip([=](View<accessor,double,3> v1){return
		[=](View<accessor,double,2> v2){return
		[=](View<accessor,double,3,2> result){
		Map([=](View<accessor,double> x){return
		[=](View<accessor,double,2> result){
		[=](View<accessor,double> x){return
		[=](View<accessor,double,2> v){return
		[=](View<accessor,double,2> result){
		Map([=](View<accessor,double> x){return
		[=](View<accessor,double> y){return
		[=](View<accessor,double> result){
		result=x*y;};};}(x),v,result);};};}(x)(v2)(result);};},v1,result);};};},m,v_mat8,v_2061724296[thread_id]),
		Reduce([=](View<accessor,double,3,2> m1){return
		[=](View<accessor,double,3,2> m2){return
		[=](View<accessor,double,3,2> result){
		Zip([=](View<accessor,double,2> v1){return
		[=](View<accessor,double,2> v2){return
		[=](View<accessor,double,2> result){
		Zip([=](View<accessor,double> x){return
		[=](View<accessor,double> y){return
		[=](View<accessor,double> result){
		result=x+y;};};},v1,v2,result);};};},m1,m2,result);};};},v_2061724296[thread_id],result);};},v_tens,v_2147482884,8);
	});
	return result;
}
