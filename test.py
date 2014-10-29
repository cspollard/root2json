from sys import argv
from rootpy import ROOT
from rootpy.tree import Tree

fin = ROOT.TFile(argv[1])
ftemp = ROOT.TFile("test.root", "recreate")
tin = Tree(fin.Get(argv[2]))

print dir(tree)
