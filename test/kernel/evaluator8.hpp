#include "helper.h"
#include "my_sycl.h"
#include "View.h"
#include <map>
#include <string>

View<double*,double,2,3,2> evaluator8(std::map<std::string, double*> bigVectors){
	View<double*,double,2,3,2> result(std::array<size_t,3>{6,2,1}, new double[12]);
	buffer_t b_mat8496183094(const_cast<const double*>(bigVectors.at("mat8")),8);
	buffer_t b_tens1838073155(const_cast<const double*>(bigVectors.at("tens")),24);
	buffer_t b_2147482884(result.data, 12);
	buffer_t b_844138389(96);
	cl::sycl::queue deviceQueue;
	deviceQueue.submit([&] (cl::sycl::handler &cgh) {
		act_cgh = &cgh;
		View<accessor,double,4,2> mat8496183094(std::array<size_t,2>{2,1},b_mat8496183094.get_access<rw_access>(cgh));
		View<accessor,double,2,4,3> tens1838073155(std::array<size_t,3>{12,1,4},b_tens1838073155.get_access<rw_access>(cgh));
		View<accessor,double,2,3,2> v_2147482884(std::array<size_t,3>{6,2,1},b_2147482884.get_access<rw_access>(cgh));
		View<accessor,double,4,4,3,2> v_844138389(std::array<size_t,4>{24,6,2,1},b_844138389.get_access<rw_access>(cgh));
		ParMap([=](auto m){return
		[=](auto result, unsigned thread_id){
		Zip([=](auto v1){return
		[=](auto v2){return
		[=](auto result){
		Map([=](auto x){return
		[=](auto result){
		[=](auto x){return
		[=](auto v){return
		[=](auto result){
		Map([=](auto x){return
		[=](auto y){return
		[=](auto result){
		result=x*y;};};}(x),v,result);};};}(x)(v2)(result);};},v1,result);};};},m,mat8496183094,v_844138389[thread_id]),
		Reduce([=](auto m1){return
		[=](auto m2){return
		[=](auto result){
		Zip([=](auto v1){return
		[=](auto v2){return
		[=](auto result){
		Zip([=](auto x){return
		[=](auto y){return
		[=](auto result){
		result=x+y;};};},v1,v2,result);};};},m1,m2,result);};};},v_844138389[thread_id],result);};},tens1838073155,v_2147482884,4);
	});
	return result;
}
