from sys import argv, stdout, stderr
from array import array
import ROOT
from string import count
from math import isnan


drootconv = {
        "Int_t" : "int",
        "UInt_t" : "unsigned int",
        "ULong64_t" : "unsigned long",
        "Char_t" : "char",
        "Float_t" : "float",
        "Double_t" : "double",
        "Bool_t" : "bool"
        }

darrconv = {
        "int": 'i',
        "unsigned int": 'I',
        "unsigned long": 'L',
        "char": 'c',
        "float": 'f',
        "double": 'd',
        "bool": 'i'
        }


def showBranch(bval, printer):
    return printer(bval)


def showEvt(branches, stream):
    if not len(branches):
        return "[]"

    stream.write(
            "[%s" % branches[0][2](branches[0][1])
            )

    for (bname, bval, bprinter) in branches[1:]:
        stream.write(
                ",%s" % bprinter(bval)
                )

    stream.write("]")
    return

def showObj(x, isChar=False):
    if isChar:
        return str(ord(x))
    if type(x) == float and isnan(x):
        return '"NaN"'
    return str(x)

def vector2string(n=1, isChar=False):

    # this is the bottleneck function.
    def f(v):
        if n == 0:
            return showObj(v[0], isChar=isChar)

        if not v.size():
            return "[]"

        ss = ["["]
        if n > 1:
            ss.append(vector2string(n-1, isChar=isChar)(v[0]))
            for x in v[1:]:
                ss.append(",")
                ss.append(vector2string(n-1, isChar=isChar)(x))
        else:
            ss.append(showObj(v[0], isChar=isChar))
            for x in v[1:]:
                ss.append(",")
                ss.append(showObj(x, isChar=isChar))

        ss.append("]")

        return "".join(ss)

    return f



def get_type_obj(classname):
    for k in reversed(sorted(drootconv.keys())):
        if k in classname:
            classname = classname.replace(k, drootconv[k])

    if "vector" in classname:
        return ROOT.std.vector(classname.replace("vector", "", 1).strip(" <").rstrip(" >"))()
    else:
        if classname == "char":
            c = "0"
        else:
            c = [0]
        return array(darrconv[classname], c)


def JSONDumpTree(tree, branches_on=None, stream=stdout):

    stream.write('{\n\t"branches" : [\n')

    lleaves = []
    if branches_on is None:
        for b in tree.GetListOfBranches():
            for l in b.GetListOfLeaves():
                lleaves.append(l)
    else:
        for bname in branches_on:
            l = tree.GetLeaf(bname)
            if not l:
                stderr.write("cannot find branch %s!!\n" % bname)
                continue

            else:
                lleaves.append(tree.GetLeaf(bname))
                continue

    tree.SetBranchStatus("*", 0)
    for l in lleaves:
        tree.SetBranchStatus(l.GetName(), 1)

    lbranches = []
    for l in lleaves[:-1]:
        lname = l.GetName()
        lclass = l.GetTypeName()

        stream.write('\t\t["%s", "%s"],\n' % (lname, lclass))

        type_obj = get_type_obj(lclass)
        printer_obj = vector2string(count(lclass, "vector"),
                isChar=("char" in lclass or "Char" in lclass))
        lbranches.append((lname, type_obj, printer_obj))

        tree.SetBranchAddress(lname, type_obj)

    l = lleaves[-1]
    lname = l.GetName()
    lclass = l.GetTypeName()

    stream.write('\t\t["%s", "%s"]\n\t],\n' % (lname, lclass))

    type_obj = get_type_obj(lclass)
    printer_obj = vector2string(count(lclass, "vector"),
            isChar=("char" in lclass or "Char" in lclass))
    lbranches.append((lname, type_obj, printer_obj))

    tree.SetBranchAddress(lname, type_obj)

    nentries = tree.GetEntries()
    stream.write('\t"events" : [\n')

    if not nentries:
        stream.write(']}')
        return

    tree.GetEntry(0)
    showEvt(lbranches, stream)

    for entry in xrange(1, nentries):
        tree.GetEntry(entry)
        stream.write(",\n")
        showEvt(lbranches, stream)

    stream.write("\n]\n}")

    return

if __name__ == "__main__":
    fin = ROOT.TFile(argv[1])
    treesBranches = argv[2:]

    stdout.write("{\n")
    for (i, s) in enumerate(treesBranches):
        if i > 0:
            stdout.write(",\n")

        if ":" in s:
            tn, bf = s.split(":", 1)
            fbs = open(bf, 'r')
            branches = fbs.readlines()
            branches_on = []
            for b in branches:
                b = b.strip()
                if b and not b.startswith("#"):
                    branches_on.append(b)
                else:
                    continue
            fbs.close()
        else:
            tn = s
            branches_on = None

        tin = fin.Get(tn)
        stdout.write('"%s" : ' % tin.GetName())
        JSONDumpTree(tin, branches_on=branches_on)

    stdout.write("\n}")
    fin.Close()
