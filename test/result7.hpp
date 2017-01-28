#include "helper.h"
#include "View.h"
#include <map>
#include <string>
#include <vector>

auto evaluator(std::map<std::string, double*> bigVectors){
	View<double,Pair<2,2>,Pair<2,1>> s2147482884;
	View<double,Pair<3,1>> s203958725[8];
	([&](const auto& m1){return
	[&](const auto& m2){return
	[&](const auto& result){
	ParMap([&](const auto& v){return
	[&](const auto& result, unsigned thread_id){
	Map(([&](const auto& v1){return
	[&](const auto& v2){return
	[&](const auto& result){
	Zip([&](const auto& x){return
	[&](const auto& y){return
	[&](const auto& result){
	result=x*y;};};},v1,v2,s203958725[thread_id]),
	Reduce([&](const auto& x){return
	[&](const auto& y){return
	[&](const auto& result){
	result=x+y;};};},s203958725[thread_id],result);};};}(v)),m2,result);};},m1,result,8);};};}(View<double,Pair<2,3>,Pair<3,1>>(bigVectors.at("mat"))))(View<double,Pair<2,1>,Pair<3,2>>(bigVectors.at("mat")))(s2147482884);
	return s2147482884;
}
