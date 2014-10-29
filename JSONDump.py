from sys import argv
import ROOT

def JSONDump(tree):
    for b in tree.GetListOfBranches():
        print b.GetName()
        print b.GetClassName()
        for l in b.GetListOfLeaves():
            print l.GetName()
            print l.GetTypeName()


if __name__ == "__main__":
    fin = ROOT.TFile(argv[1])
    tin = fin.Get(argv[2])

    JSONDump(tin)
