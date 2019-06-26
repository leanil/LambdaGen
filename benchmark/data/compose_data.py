import itertools
import json
import os
import sys

def merge_json(expr, time, id):
    result = {} # create a new dict to set custom order
    result["id"] = id
    result["name"] = expr["name"]
    if result["name"] != time["run_name"][0:-10]:
        raise AssertionError("measurement name mismatch")
    result["pretty"] = expr["pretty"]
    result["time"] = time["cpu_time"]
    result["count"] = expr["count"]
    result["cost"],result["value"] = 0,0
    result["tree"] = expr["tree"]
    return result

with open("expressions.json") as expr_file:
    expr_str = expr_file.readline()
exprs = json.loads(expr_str)
context = None
times = []
for time_filename in sys.argv[1:]:
    with open(time_filename) as time_file:
        time_str = time_file.read()
    time_json = json.loads(time_str)
    times += [i for i in time_json["benchmarks"] if i["aggregate_name"] == "min"]
    if not context:
        context = time_json["context"]
if len(exprs) != len(times):
    print( len(exprs) ,len(times))
    raise AssertionError("measurement count mismatch")
data = map(merge_json, exprs, times, itertools.count())
with open("data.json", 'w') as data_file:
    data_file.write('{\n"data" :\n[\n')
    first = True
    for d in data:
        if first:
            first = False
        else:
            data_file.write(",\n")
        data_file.write(json.dumps(d))
    data_file.write('\n],\n"context":' + json.dumps(context,indent=2) + '\n}\n')
print("Composed data has been written to:", os.path.join(os.getcwd(),"data.json"), sep='\n')