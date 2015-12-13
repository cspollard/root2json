from sys import argv, stdout
from array import array
import ROOT
from string import count


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
            "[ %s" % branches[0][2](branches[0][1])
            )

    for (bname, bval, bprinter) in branches[1:]:
        stream.write(
                ", %s" % bprinter(bval)
                )

    stream.write(" ]")
    return


def vector2string(n=1):

    def f(v):
        if n == 0:
            return str(v[0])

        if not v.size():
            return "[]"

        ss = ["["]
        if n > 1:
            ss.append(vector2string(n-1)(v[0]))
            for x in v[1:]:
                ss.append(", ")
                ss.append(vector2string(n-1)(x))
        else:
            ss.append(str(v[0]))
            for x in v[1:]:
                ss.append(", ")
                ss.append(str(x))

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
        return array(darrconv[classname], [0])


def JSONDump(tree, branches_on=None, stream=stdout):

    stream.write('{\n\t"branches" : [\n')

    lleaves = []
    if branches_on is None:
        for b in tree.GetListOfBranches():
            for l in b.GetListOfLeaves():
                lleaves.append(l)
    else:
        for bname in branches_on:
            lleaves.append(tree.GetLeaf(bname))

    tree.SetBranchStatus("*", 0)
    map(lambda l: tree.SetBranchStatus(l.GetName(), 1), lleaves)

    lbranches = []
    for l in lleaves[:-1]:
        lname = l.GetName()
        lclass = l.GetTypeName()

        stream.write('\t\t["%s", "%s"],\n' % (lname, lclass))

        type_obj = get_type_obj(lclass)
        printer_obj = vector2string(count(lclass, "vector"))
        lbranches.append((lname, type_obj, printer_obj))

        tree.SetBranchAddress(lname, type_obj)

    l = lleaves[-1]
    lname = l.GetName()
    lclass = l.GetTypeName()

    stream.write('\t\t["%s", "%s"]\n\t],\n' % (lname, lclass))

    type_obj = get_type_obj(lclass)
    printer_obj = vector2string(count(lclass, "vector"))
    lbranches.append((lname, type_obj, printer_obj))

    tree.SetBranchAddress(lname, type_obj)


    nentries = tree.GetEntries()
    stream.write('\t"events" : [\n')

    if not nentries:
        stream.write('\t]}')
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
    tin = fin.Get(argv[2])

    JSONDump(tin)
