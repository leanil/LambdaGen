#include "helper.h"
#include "View.h"
#include <map>
#include <string>
#include <vector>

auto evaluator(std::map<std::string, double*> bigVectors){
	View<double,Pair<3,3>,Pair<3,1>> s2147482884;
	([&](const auto& v1){return
	[&](const auto& v2){return
	[&](const auto& result){
	ParMap([&](const auto& x){return
	[&](const auto& result, unsigned thread_id){
	([&](const auto& x){return
	[&](const auto& v){return
	[&](const auto& result){
	Map(([&](const auto& x){return
	[&](const auto& y){return
	[&](const auto& result){
	result=x*y;};};}(x)),v,result);};};}(x))(v2)(result);};},v1,result,8);};};}(View<double,Pair<3,1>>(bigVectors.at("a"))))(View<double,Pair<3,1>>(bigVectors.at("b")))(s2147482884);
	return s2147482884;
}
