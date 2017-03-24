load("../../DUMP/data/brainspan/newGenexp.RData")
rownames(genexp) <- genexp[,1]
genexp <- genexp[,-1]


#tabulate the region
