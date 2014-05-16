import json
from sys import argv, stdout
from root_numpy import root2array
import numpy as np

def npdumps(obj):
    return json.dumps(obj, cls=NPJSONEncoder)

class NPJSONEncoder (json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, np.ndarray):
            return map(self.default, obj)
        else:
            return np.asscalar(obj)

filename = argv[1]

if len(argv) > 2:
    treename = argv[2]
else:
    treename = None

arr = root2array(filename, treename)

names = list(arr.dtype.names)

output = [names, arr.tolist()]
print json.dumps(output, cls=NPJSONEncoder)
