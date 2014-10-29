import json
from sys import argv, stdout
from root_numpy import root2array
import numpy as np
import gzip

"""
def get_kind(dt):
    k = ''
    if dt.kind == 'O':
        return 'V' + get_kind(
"""

class NPJSONEncoder (json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, np.ndarray):
            return map(self.default, obj)
        else:
            return np.asscalar(obj)

if len(argv) < 3:
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

print arr.dtype

nametypes = [[name, fields[name][0].kind] for name in names]

print nametypes;

exit()

fout = gzip.open(outfilename, 'wb')
fout.write(json.dumps(nametypes, cls=NPJSONEncoder))
fout.write('\n')
for evt in arr:
    fout.write(json.dumps(evt, cls=NPJSONEncoder))
    fout.write('\n')
fout.close()
