import json
from sys import argv, stdout
from root_numpy import root2array
import numpy as np
import gzip

class NPJSONEncoder (json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, np.ndarray):
            return map(self.default, obj)
        else:
            return np.asscalar(obj)

if len(argv) < 2:
    print "usage:", argv[0], "infilename [treename] outfilname"
    exit(-1)

infilename = argv[1]

if len(argv) > 3:
    treename = argv[2]
else:
    treename = None

outfilename = argv[-1]

arr = root2array(infilename, treename=treename)

names = arr.dtype.names
fields = arr.dtype.fields

nametypes = [[name, fields[name][0].str] for name in names]

output = {"branches" : nametypes, "events" : arr}

fout = gzip.open(outfilename, 'wb')
fout.write(json.dumps(output, cls=NPJSONEncoder))
fout.close()
