#include "helper.h"
#include "View.h"
#include <map>
#include <string>
#include <vector>

auto evaluator(std::map<std::string, double*> bigVectors){
	View<double,Pair<2,6>,Pair<3,2>,Pair<2,1>> s2147482884;
	View<double,Pair<4,6>,Pair<3,2>,Pair<2,1>> s2061724296[8];
	ParMap([&](const auto& m){return
	[&](const auto& result, unsigned thread_id){
	Zip([&](const auto& v1){return
	[&](const auto& v2){return
	[&](const auto& result){
	Map([&](const auto& x){return
	[&](const auto& result){
	([&](const auto& x){return
	[&](const auto& v){return
	[&](const auto& result){
	Map(([&](const auto& x){return
	[&](const auto& y){return
	[&](const auto& result){
	result=x*y;};};}(x)),v,result);};};}(x))(v2)(result);};},v1,result);};};},m,View<double,Pair<4,2>,Pair<2,1>>(bigVectors.at("mat8")),s2061724296[thread_id]),
	Reduce([&](const auto& m1){return
	[&](const auto& m2){return
	[&](const auto& result){
	Zip([&](const auto& v1){return
	[&](const auto& v2){return
	[&](const auto& result){
	Zip([&](const auto& x){return
	[&](const auto& y){return
	[&](const auto& result){
	result=x+y;};};},v1,v2,result);};};},m1,m2,result);};};},s2061724296[thread_id],result);};},View<double,Pair<2,12>,Pair<4,1>,Pair<3,4>>(bigVectors.at("tens")),s2147482884,8);
	return s2147482884;
}
