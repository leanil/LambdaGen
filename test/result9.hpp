#include "helper.h"
#include "View.h"
#include <map>
#include <string>
#include <vector>

auto evaluator(std::map<std::string, double*> bigVectors){
	View<double,Pair<3,1>> s2147482884;
	View<double,Pair<3,1>> s898856380;
	ParMap([&](const auto& x){return
	[&](const auto& result, unsigned thread_id){
	result=3.0*x;};},View<double,Pair<3,1>>(bigVectors.at("a")),s898856380,8),
	ParZip([&](const auto& x){return
	[&](const auto& y){return
	[&](const auto& result, unsigned thread_id){
	result=x+y;};};},s898856380,View<double,Pair<3,1>>(bigVectors.at("b")),s2147482884,8);
	return s2147482884;
}
