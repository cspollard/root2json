# Chris Pollard, Duke University
# cpollard at cern.ch

from sys import argv
from re import search, findall, sub

ElectronBranches = ["n", "cl_E", "cl_eta", "cl_phi",
        "trackpt", "tracketa", "trackphi", "author", "OQ",
        "trackz0pvunbiased", "tightPP", "mediumPP", "loosePP",
        "trackd0", "trackz0", "nSiHits", "pt", "eta", "phi",
        "MI10_max30_ptsum", "MI10_max40_ptsum", "MI10_max30_nTrks",
        "MI10_max40_nTrks", "MI15_max30_ptsum", "MI15_max40_ptsum",
        "MI15_max30_nTrks", "MI15_max40_nTrks", "Etcone20",
        "Etcone30", "Etcone40", "ptcone20", "ptcone30", "ptcone40",
        "charge", "isEM", "convmatch", "type", "origin",
        "MET_tightpp_wet", "MET_tightpp_wpx",
        "MET_tightpp_wpy", "MET_tightpp_statusWord",
        "GSF_trk_index"]

MuonBranches = ["n", "pt", "eta", "phi", "author",
        "MI10_max30_ptsum", "MI10_max40_ptsum", "MI10_max30_nTrks",
        "MI10_max40_nTrks", "MI15_max30_ptsum", "MI15_max40_ptsum",
        "MI15_max30_nTrks", "MI15_max40_nTrks", "etcone20",
        "etcone30", "etcone40", "ptcone20", "ptcone30", "ptcone40",
        "charge", "nBLHits", "nPixHits", "nPixelDeadSensors",
        "nSCTHits", "nSCTHoles", "nSCTDeadSensors", "nPixHoles",
        "nTRTHits", "nTRTOutliers", "id_z0_exPV", "expectBLayerHit",
        "ms_qoverp", "ms_theta", "ms_phi",
        "me_qoverp", "me_theta", "me_phi",
        "id_qoverp", "id_theta", "id_phi",
        "trackd0pvunbiased", "tracksigd0pvunbiased",
        "MET_tightpp_wet",
        "MET_tightpp_wpx",
        "MET_tightpp_wpy",
        "MET_tightpp_statusWord",
        "type", "origin"]

JetBranches = ["n", "pt", "eta", "phi", "E", "flavor_weight_SV0",
        "flavor_weight_MV1", "flavor_weight_JetFitterCOMBNN",
        "isBadLooseMinus", "jvtxf", "flavor_truth_label",
        "flavor_truth_dRminToB",
        "constscale_E", "constscale_eta", "constscale_phi",
        "constscale_m",
        "emscale_E", "emscale_eta", "emscale_phi",
        "emscale_m",
        "MET_tightpp_wet",
        "ActiveAreaPx", "ActiveAreaPy", "ActiveAreaPz",
        "ActiveAreaE",
        "MET_tightpp_wpx",
        "MET_tightpp_wpy",
        "MET_tightpp_statusWord",
        "sumPtTrk_pv0_500MeV", "TrackAssoc_index"]

TruthJetBranches = ["n", "pt", "eta", "phi", "E",
        "flavor_truth_label", "flavor_truth_dRminToB"]

FatJetBranches = ["n", "pt", "eta", "phi", "E", "SPLIT12", "QW",
        "Tau1", "Tau2", "Tau3",
        "constscale_E", "constscale_eta", "constscale_phi",
        "constscale_m",
        "emscale_E", "emscale_eta", "emscale_phi",
        "emscale_m"]

ElectronTriggerBranches = ["n", "pt", "eta", "phi", "E",
        "EF_e24vhi_medium1", "EF_e60_medium1"]

MuonTriggerBranches = ["n", "track_n", "track_CB_pt", "track_CB_eta",
        "track_CB_phi", "EF_mu24i_tight", "EF_mu36_tight"]

TruthParticleBranches = ["n", "pt", "m", "eta", "phi", "status",
        "pdgId", "parents", "children"]

TrackBranches = ["n", "pt", "eta", "phi_wrtPV", "theta_wrtPV",
        "z0_wrtPV", "d0_wrtPV"]

EventBranches = ["RunNumber", "EventNumber", "mc_channel_number",
        "averageIntPerXing", "larError", "tileError", "coreFlags",
        "top_hfor_type", "EF_mu24i_tight", "EF_mu36_tight", "lbn",
        "EF_e24vhi_medium1", "EF_e60_medium1",
        "MET_SoftJets_etx",
        "MET_SoftJets_ety",
        "MET_SoftJets_sumet",
        "MET_SoftJets_et",
        "MET_CellOut_tightpp_etx",
        "MET_CellOut_tightpp_ety",
        "MET_CellOut_tightpp_sumet",
        "MET_CellOut_tightpp_et",
        "MET_RefEle_et",
        "MET_Muon_Total_Muons_et",
        "MET_RefJet_et",
        "MET_RefFinal_tightpp_etx",
        "MET_RefFinal_tightpp_ety",
        "mcevt_weight", "vxp_n", "vxp_type",
        "vxp_nTracks",
        # "vxp_trk_n",
        "mcVx_z", "Eventshape_rhoKt4LC",
        "GSF_trk_trk_index"]

def ttMakeCollectionClass(obj_name, tree_header_fname,
        var_list, prefix=""):
    inf = open(tree_header_fname)
    inlines = inf.readlines()
    inf.close()

    tree_variables = set()
    for var_name in var_list:
        for line in inlines:
            s = search("[ *]" + prefix + var_name + ";", line)
            if s:
                var_type = s.string[:s.start()+1].strip()
                tree_variables.add((var_type, var_name))
                break

    # recast items as lists so they are mutable.
    tree_variables = map(list, tree_variables)
    tree_variables.sort(cmp=lambda x, y: cmp(x[1], y[1]))

    for var_info in tree_variables:
        var_info[0] = sub("\s+", " ", var_info[0])

    var_list = map(lambda x: x[1], tree_variables)

    # back to tuples for string formatting.
    tree_variables = map(tuple, tree_variables)

    collection_name = "tt%sD3PDCollection" % obj_name
    outf = open(collection_name + ".h", 'w')

    outf.write("// Chris Pollard, Duke University\n// cpollard at cern.ch\n\n")
    outf.write("#ifndef __%s_H__\n#define __%s_H__\n\n" %
            (collection_name.upper(), collection_name.upper()))
    outf.write("#include \"TBranch.h\"\n#include \"TTree.h\"\n")
    outf.write("#include <vector>\n#include <string>\n\n")

    outf.write("class %s {\n\tpublic:\n" % collection_name)
    outf.write("\t\t%s() { }\n" % collection_name)
    outf.write("\n\t\t%s(const std::string &prefix) : fPre(prefix) { }\n" %
            collection_name)
    outf.write("\t\t~%s() { }\n" % collection_name)

    outf.write("\t\tvoid Init(TTree *);\n\n")
    outf.write("\t\t// tree and variables.\n\t\tTTree *fTree;\n")
    outf.write("\t\tstd::string fPre;\n\n")

    for var_info in tree_variables:
        if var_info[0].endswith("*"):
            outf.write(("\t\t%s%s;\n" % var_info).replace("vector",
                "std::vector"))
        else:
            outf.write(("\t\t%s %s;\n" % var_info).replace("vector",
                "std::vector"))

    outf.write("\n\t\t// setup branches.\n")
    for var_name in var_list:
        outf.write("\t\tTBranch *b_%s;\n" % var_name)

    outf.write("};\n\n")
    outf.write("#endif")

    outf.close()

    outf = open(collection_name + ".cxx", 'w')
    outf.write("#include \"%s.h\"\n\n" % collection_name)
    outf.write("void %s::Init(TTree *tree) {\n" % collection_name)
    outf.write("\tfTree = tree;\n\n")

    outf.write("\t// turn on branches.\n")
    for var_name in var_list:
        outf.write("\tfTree->SetBranchStatus((fPre + \"%s\").c_str(), 1);\n" %
                var_name)

    outf.write("\n\t// Reset variables.\n")
    for var_name in var_list:
        outf.write("\t%s = 0;\n" % var_name)


    outf.write("\n\t// Set branch addresses.")
    for var_name in var_list:
        outf.write("\n\tfTree->SetBranchAddress((fPre ")
        outf.write("+ \"%s\").c_str(), &%s, &b_%s);" %
                (var_name, var_name, var_name))

    outf.write("\n\n\treturn;\n}")
    outf.close()


if __name__=="__main__":
    if len(argv) < 2:
        ttMakeCollectionClass("Electron", "NTUP_COMMON.h",
                ElectronBranches, "el_")
        ttMakeCollectionClass("Muon", "NTUP_COMMON.h",
                MuonBranches, "mu_muid_")
        ttMakeCollectionClass("Jet", "NTUP_COMMON.h",
                JetBranches, "jet_AntiKt4LCTopo_")
        ttMakeCollectionClass("FatJet", "NTUP_COMMON.h",
                FatJetBranches, "jet_AntiKt10LCTopoTrimmedPtFrac5SmallR30_")
        ttMakeCollectionClass("TruthJet", "NTUP_COMMON.h",
                TruthJetBranches, "AntiKt4Truth_")
        ttMakeCollectionClass("ElectronTrigger", "NTUP_COMMON.h",
                ElectronTriggerBranches, "trig_EF_el_")
        ttMakeCollectionClass("MuonTrigger", "NTUP_COMMON.h",
                MuonTriggerBranches, "trig_EF_trigmuonef_")
        ttMakeCollectionClass("Track", "NTUP_COMMON.h",
                TrackBranches, "trk_")
        ttMakeCollectionClass("TruthParticle", "NTUP_COMMON.h",
                TruthParticleBranches, "mc_")
        ttMakeCollectionClass("Event", "NTUP_COMMON.h",
                EventBranches, "")

    elif argv[1] == "Electron":
        ttMakeCollectionClass("Electron", "NTUP_COMMON.h",
                ElectronBranches, "el_")
    elif argv[1] == "Muon":
        ttMakeCollectionClass("Muon", "NTUP_COMMON.h",
                MuonBranches, "mu_muid_")
    elif argv[1] == "Jet":
        ttMakeCollectionClass("Jet", "NTUP_COMMON.h",
                JetBranches, "jet_AntiKt4LCTopo_")
    elif argv[1] == "TruthJet":
        ttMakeCollectionClass("TruthJet", "NTUP_COMMON.h",
                TruthJetBranches, "AntiKt4Truth_")
    elif argv[1] == "FatJet":
        ttMakeCollectionClass("FatJet", "NTUP_COMMON.h",
                FatJetBranches, "jet_AntiKt10LCTopoTrimmedPtFrac5SmallR30_")
    elif argv[1] == "ElectronTrigger":
        ttMakeCollectionClass("ElectronTrigger", "NTUP_COMMON.h",
                ElectronTriggerBranches, "trig_EF_el_")
    elif argv[1] == "MuonTrigger":
        ttMakeCollectionClass("MuonTrigger", "NTUP_COMMON.h",
                MuonTriggerBranches, "trig_EF_trigmuonef_")
    elif argv[1] == "TruthParticle":
        ttMakeCollectionClass("TruthParticle", "NTUP_COMMON.h",
                TruthParticleBranches, "mc_")
    elif argv[1] == "Track":
        ttMakeCollectionClass("Track", "NTUP_COMMON.h",
                TrackBranches, "trk_")
    elif argv[1] == "Event":
        ttMakeCollectionClass("Event", "NTUP_COMMON.h",
                EventBranches, "")

    else:
        print argv[1], "is not a recognized collection. Doing nothing."
