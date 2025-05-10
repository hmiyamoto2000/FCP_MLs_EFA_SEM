#For EFA
dir.create("./EFA_result", showWarnings = TRUE, recursive = FALSE, mode = "0777")

library( psych )
library( GPArotation )

data=read.csv("FCPC_MLs_AA.csv")
KMO(data)

k_d=KMO(data)
sink('./EFA_result/KMO.txt', append = TRUE)
print (k_d)
sink()

#par("mar"=c(1,1,1,1))
fa.parallel(data, fa = "fa", use = "complete.obs")
abline(h = 0)
parallel=fa.parallel(data, fa = "fa", use = "complete.obs")
abline(h = 0)
print(parallel)

sink('./EFA_result/parallel.txt', append = TRUE)
print (parallel)
sink()

MAPminres <- vss(data, fm="minres")
print(MAPminres)

sink('./EFA_result/VSS_MAPminres.txt', append = TRUE)
print (MAPminres)
sink()

#Calculate as factor=8 based on the data using the function "vss"
#Use rotate = "promax" based on the result using CA
result_7 = fa(data, nfactors = 7, fm = "minres", rotate = "promax", use = "complete.obs" )
print( result_7, digits = 3, sort = T)

sink('./EFA_result/nfactors_7.txt', append = TRUE)
print (result_7)
sink()

library(heatmaply)

heatmaply(fa(data, nfactors = 7, fm = "minres", rotate = "promax")$loadings,grid_gap = 1,subplot_widths = c(0.3, 0.2),subplot_heights = c(0.20, 0.70))
