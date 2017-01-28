#include "helper.h"
#include "View.h"
#include <map>
#include <string>
#include <vector>

auto evaluator(std::map<std::string, double*> bigVectors){
	View<double,Pair<2,1>> s2147482884;
	View<double,Pair<3,1>> s332304331[8];
	ParMap(([&](const auto& v1){return
	[&](const auto& v2){return
	[&](const auto& result, unsigned thread_id){
	Zip([&](const auto& x){return
	[&](const auto& y){return
	[&](const auto& result){
	result=x*y;};};},v1,v2,s332304331[thread_id]),
	Reduce([&](const auto& x){return
	[&](const auto& y){return
	[&](const auto& result){
	result=x+y;};};},s332304331[thread_id],result);};};}(View<double,Pair<3,1>>(bigVectors.at("vec")))),View<double,Pair<2,3>,Pair<3,1>>(bigVectors.at("mat")),s2147482884,8);
	return s2147482884;
}
